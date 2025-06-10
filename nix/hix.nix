{ pkgs, ... }:
rec {
  name = "mcp-selenium-haskell";
  compiler-nix-name = "ghc910"; # Version of GHC to use

  crossPlatforms =
    _p:
    pkgs.lib.optionals pkgs.stdenv.hostPlatform.isx86_64 (
      [
      ]
      ++ pkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux [
        # p.musl64
      ]
    );

  # Tools to include in the development shell
  shell.tools.cabal = "latest";
  shell.tools.hlint = "latest";
  shell.tools.ormolu = "latest";
  shell.tools.haskell-language-server = "latest";
  # Add non-Haskell tools to the development shell
  shell.buildInputs =
    with pkgs;
    with python3.pkgs;
    [
      python
      pytest
      pytest-asyncio
      mcp
    ];
}
