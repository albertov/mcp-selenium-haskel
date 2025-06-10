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
          name = "mcp-selenium-haskell-integration-test";
          runtimeInputs = with pkgs; [
            selenium-server-standalone
            chromium
            python3
          ];
          text =
            let
              py_path =
                with pkgs.python3.pkgs;
                lib.makeSearchPath "lib/python3.12/site-packages" [
                  pytest
                  pytest-asyncio
                  mcp
                  pluggy
                  iniconfig
                  python-dotenv
                  anyio
                  sniffio
                  typing-extensions
                  typing-inspection
                  annotated-types
                  httpx
                  httpx-sse
                  idna
                  pydantic
                  pydantic-core
                  pydantic-settings
                  starlette
                  sse-starlette
                ];
            in
            ''
              export MCP_SELENIUM_EXE=${self.packages.${system}.mcp-selenium-hs}/bin/mcp-selenium-hs
              export PYTHONPATH="${self}/tests:${py_path}"
              exec python3 ${self}/orchestrate_integration_tests.py
            '';
        };
      in
      pkgs.lib.recursiveUpdate flake {
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
      }
    );

  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    extra-substituters = [ "https://mcp-selenium-haskell.cachix.org" ];
    extra-trusted-public-keys = [ "mcp-selenium-haskell.cachix.org-1:C+mSRd39ugTt5+QWvgPRVmGYnHBMFu0+8HW0oW8uA+Y=" ];
    allow-import-from-derivation = "true";
  };
}
