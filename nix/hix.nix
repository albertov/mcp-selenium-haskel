{ pkgs, ... }:
rec {
  name = "mcp-selenium-haskell";
  compiler-nix-name = "ghc910"; # Version of GHC to use
  index-state = "2025-06-09T20:31:03Z";
  # This must be updated when haskell dependencies change
  plan-sha256 = "sha256-jd+Kx7EB4EHPqqguQzjpr7PMhnpCm7Ortpy2Pvq1Oeg=";

  # Tools to include in the development shell
  shell.tools.cabal = "latest";
  shell.tools.hlint = "latest";
  shell.tools.ormolu = "latest";
  shell.tools.haskell-language-server = "latest";
  # Non-Haskell tools to include in the development shell
  shell.nativeBuildInputs =
    with pkgs.buildPackages;
    with python3.pkgs;
    [
      chromium
      selenium-server-standalone
      python
      pytest
      pytest-asyncio
      mcp
    ];
}
