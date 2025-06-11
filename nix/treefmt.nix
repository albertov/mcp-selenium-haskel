# treefmt.nix
{ ... }:
{
  # Used to find the project root
  settings.global.excludes = [ "nix/materialized/**" ];
  projectRootFile = "flake.nix";
  programs.nixfmt.enable = true;
  programs.ormolu.enable = true;
  programs.shellcheck.enable = true;
  programs.deadnix.enable = true;
  programs.ruff.enable = true;
}
