# treefmt.nix
{ ... }:
{
  # Used to find the project root
  projectRootFile = "flake.nix";
  programs.nixfmt.enable = true;
  programs.ormolu.enable = true;
  programs.shellcheck.enable = true;
  programs.deadnix.enable = true;
}
