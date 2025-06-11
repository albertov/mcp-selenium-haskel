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

        #FIXME: Migrate this back to use a writeShellApplication that has the
        # needed packages in a pythonEnv as runtimeInput. Integration tests need
        # that
        integration-tests = pkgs.python3.pkgs.buildPythonApplication {
          pname = "mcp-selenium-integration-tests";
          version = "0.1.0";
          src = self;
          format = "pyproject";

          build-system = with pkgs.python3.pkgs; [
            hatchling
          ];

          propagatedBuildInputs = with pkgs.python3.pkgs; [
            pytest
            pytest-asyncio
            mcp
          ];

          nativeBuildInputs = [ pkgs.makeWrapper ];

          postFixup = ''
            wrapProgram $out/bin/mcp-selenium-integration-tests \
              --set MCP_SELENIUM_EXE ${self.packages.${system}.mcp-selenium-hs}/bin/mcp-selenium-hs \
              --prefix PATH : ${
                pkgs.lib.makeBinPath [
                  pkgs.selenium-server-standalone
                  pkgs.chromium
                ]
              } \
              --prefix PYTHONPATH : "$out/${pkgs.python3.sitePackages}:$out/lib/${pkgs.python3.libPrefix}/site-packages"
          '';
        };
      in
      (pkgs.lib.recursiveUpdate flake {
        legacyPackages = pkgs;

        packages = flake.packages // rec {
          default = mcp-selenium-hs;
          inherit (pkgs.hixProject.projectCross.musl64.hsPkgs.mcp-selenium.components.exes) mcp-selenium-hs;
          inherit integration-tests;
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
