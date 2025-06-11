{
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.treefmt-nix = {
    url = "github:numtide/treefmt-nix";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs =
    inputs@{
      self,
      nixpkgs,
      flake-utils,
      haskellNix,
      ...
    }:
    let
      supportedSystems = [
        "x86_64-linux"
        #"x86_64-darwin"
        #"aarch64-linux"
        #"aarch64-darwin"
      ];
    in
    flake-utils.lib.eachSystem supportedSystems (
      system:
      let
        overlays = [
          haskellNix.overlay
          (final: _prev: {
            inherit hoogle;
            hixProject = final.haskell-nix.hix.project {
              src = builtins.path {
                path = ./.;
                name = "source";
              };
              evalSystem = "x86_64-linux";
            };
          })
        ];
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        flake = pkgs.hixProject.flake { };

        hoogleEnv = pkgs.hixProject.ghcWithHoogle (
          _:
          builtins.attrValues (
            pkgs.lib.filterAttrs (_: p: p.isLocal or false && p.components ? library) pkgs.hixProject.hsPkgs
          )
        );
        hoogle = pkgs.writeShellApplication {
          name = "hoogle";
          runtimeInputs = [ hoogleEnv ];
          text = ''
            hoogle "$@"
          '';
        };
        treefmtEval = inputs.treefmt-nix.lib.evalModule pkgs ./nix/treefmt.nix;

        integration-tests = pkgs.writeShellApplication {
          name = "mcp-selenium-integration-tests";
          runtimeInputs = with pkgs; [
            rsync
            selenium-server-standalone
            chromium
            (python3.withPackages (
              ps: with ps; [
                pytest
                pytest-asyncio
                mcp
              ]
            ))
          ];
          text = ''
            # Create a temporary directory
            TEMP_DIR="$(mktemp -d)"
            # Set up trap to delete the temporary directory on script exit
            trap 'rm -rf "$TEMP_DIR"' EXIT
            export MCP_SELENIUM_EXE=${self.packages.${system}.mcp-selenium-hs}/bin/mcp-selenium-hs
            rsync -a ${self}/integration_tests "$TEMP_DIR"/
            chmod -R u+w "$TEMP_DIR"
            cd "$TEMP_DIR"
            exec python integration_tests/orchestrate_integration_tests.py "$@"
          '';
        };

        updateMaterialization = pkgs.writeShellApplication {
          name = "update-materialization";
          runtimeInputs = with pkgs; [
            nix
            git
          ];
          text = ''
            set -euo pipefail

            echo "🔄 Updating Nix materialization..."

            # Backup the original hix.nix
            cp nix/hix.nix nix/hix.nix.backup
            trap 'mv nix/hix.nix.backup nix/hix.nix' EXIT

            # Step 1: Temporarily disable materialization by commenting out the line
            echo "📝 Temporarily disabling materialization..."
            sed -i 's/materialized = \.\/materialized;/# materialized = \.\/materialized; # Temporarily disabled/' nix/hix.nix

            # Step 2: Try to build the plan-nix (this will use IFD but generate what we need)
            echo "🏗️ Building project plan..."
            if nix build .#hixProject.plan-nix --no-link 2>/dev/null; then
              PLAN_RESULT=$(nix build .#hixProject.plan-nix --no-link --print-out-paths)
              echo "✅ Successfully built hixProject.plan-nix"
            elif nix build .#project.plan-nix --no-link 2>/dev/null; then
              PLAN_RESULT=$(nix build .#project.plan-nix --no-link --print-out-paths)
              echo "✅ Successfully built project.plan-nix"
            else
              echo "❌ Failed to build plan-nix target. Trying alternative approach..."
              # Try the materialize target if it exists
              if nix run .#materialize 2>/dev/null; then
                echo "✅ Successfully ran materialize target"
                mv nix/hix.nix.backup nix/hix.nix
                trap - EXIT
                echo "🎉 Materialization updated successfully using materialize target!"
                exit 0
              else
                echo "❌ Failed to find suitable materialization target"
                exit 1
              fi
            fi

            # Step 3: Remove old materialized files and copy new ones
            echo "📁 Updating materialized files..."
            rm -rf nix/materialized
            mkdir -p nix/materialized
            cp -r "$PLAN_RESULT"/* nix/materialized/

            # Step 4: Restore the original hix.nix (re-enable materialization)
            echo "🔧 Re-enabling materialization..."
            mv nix/hix.nix.backup nix/hix.nix
            trap - EXIT

            # Step 5: Test that it works
            echo "🧪 Testing materialization..."
            if nix flake check --no-build 2>/dev/null; then
              echo "✅ Flake check passed"
            else
              echo "⚠️ Flake check had issues, but materialization files were updated"
            fi

            # Step 6: Commit the materialized files
            echo "📝 Committing materialized files..."
            git add nix/materialized

            if git diff --cached --quiet; then
              echo "ℹ️ No changes to commit - materialization was already up to date"
            else
              git commit -m "feat: update materialized nix files

            This updates the materialized files to match the current project
            dependencies, eliminating the need for import-from-derivation (IFD)
            during evaluation."
              echo "✅ Committed materialized files"
            fi

            echo "🎉 Materialization updated successfully!"
            echo ""
            echo "The materialized files contain pre-computed Nix expressions that represent"
            echo "your Haskell dependencies, eliminating the need for import-from-derivation"
            echo "during evaluation."
          '';
        };
      in
      (pkgs.lib.recursiveUpdate flake {
        legacyPackages = pkgs;

        packages = flake.packages // rec {
          default = mcp-selenium-hs;
          inherit (pkgs.hixProject.projectCross.musl64.hsPkgs.mcp-selenium.components.exes) mcp-selenium-hs;
          inherit integration-tests;
          inherit updateMaterialization;
        };

        formatter = treefmtEval.config.build.wrapper;

        # for `nix flake check`
        checks = {
          formatting = treefmtEval.config.build.check self;
        };
      })
      // {
        # These require IFD and we don't want that
        hydraJobs = { };
      }
    );

  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    extra-substituters = [ "https://mcp-selenium-haskell.cachix.org" ];
    extra-trusted-public-keys = [
      "mcp-selenium-haskell.cachix.org-1:C+mSRd39ugTt5+QWvgPRVmGYnHBMFu0+8HW0oW8uA+Y="
    ];
  };
}
