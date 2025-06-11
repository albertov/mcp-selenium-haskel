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
            rsync
          ];
          text = ''
            set -euo pipefail

            echo "🔄 Updating Nix materialization..."

            PLAN_RESULT="$(mktemp -d)/plan"

            # Backup the original hix.nix
            cp nix/hix.nix nix/hix.nix.backup
            trap 'mv nix/hix.nix.backup nix/hix.nix; rm $PLAN_RESULT' EXIT

            # Step 1: Temporarily disable materialization by commenting out the line
            echo "📝 Temporarily disabling materialization..."
            sed -i 's/materialized = \.\/materialized;/# materialized = \.\/materialized; # Temporarily disabled/' nix/hix.nix

            # Step 2: Try to build the plan-nix (this will use IFD but generate what we need)
            echo "🏗️ Building project plan..."
            nix build .#hixProject.plan-nix -o "$PLAN_RESULT"

            # Step 3: Remove old materialized files and copy new ones
            echo "📁 Updating materialized files..."
            rm -rf nix/materialized
            mkdir -p nix/materialized
            rsync -a "$PLAN_RESULT"/ nix/materialized/
            chmod -R u+w nix/materialized

            # Step 4: Restore the original hix.nix (re-enable materialization)
            echo "🔧 Re-enabling materialization..."
            mv nix/hix.nix.backup nix/hix.nix
            trap - EXIT

            # Step 5: Test that it works
            echo "🧪 Testing materialization..."
            git add -f nix/materialized
            if nix flake check; then
              echo "✅ Flake check passed"
              # Step 6: Commit the materialized files
              echo "📝 Committing materialized files..."
            else
              git restore --staged nix/materialized
              git checkout nix/materialized
              echo "⚠️ Flake check had issues"
            fi


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

        release-tarball = pkgs.writeShellApplication {
          name = "create-release-tarball";
          runtimeInputs = with pkgs; [
            xz
            gnutar
            gawk
          ];
          text = ''
            set -euo pipefail

            # Extract version from cabal file
            VERSION=$(awk '/^version:/ {print $2}' mcp-selenium.cabal)
            echo "📦 Creating release tarball for version $VERSION..."

            # Create temporary directory for staging
            TEMP_DIR="$(mktemp -d)"
            trap 'rm -rf "$TEMP_DIR"' EXIT

            STAGE_DIR="$TEMP_DIR/mcp-selenium-hs-$VERSION"
            mkdir -p "$STAGE_DIR"

            # Copy the executable
            echo "📁 Copying executable..."
            cp ${self.packages.${system}.mcp-selenium-hs}/bin/mcp-selenium-hs "$STAGE_DIR/"

            # Copy documentation files
            echo "📄 Copying documentation files..."
            cp README.md "$STAGE_DIR/"
            cp CHANGELOG.md "$STAGE_DIR/"
            cp LICENSE "$STAGE_DIR/LICENSE.md"
            cp TODO.md "$STAGE_DIR/"
            cp CONTRIBUTORS.md "$STAGE_DIR/"

            # Create tarball with maximum compression
            echo "🗜️ Creating compressed tarball..."
            cd "$TEMP_DIR"
            tar -cf - "mcp-selenium-hs-$VERSION" | xz -9e > "$(pwd)/mcp-selenium-hs.$VERSION.tar.xz"

            # Move to current directory
            mv "mcp-selenium-hs.$VERSION.tar.xz" "$(pwd)"
            echo "✅ Created mcp-selenium-hs.$VERSION.tar.xz"
            echo "📊 Size: $(du -h "mcp-selenium-hs.$VERSION.tar.xz" | cut -f1)"
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
          inherit release-tarball;
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
