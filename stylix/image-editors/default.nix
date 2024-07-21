{
  config,
  lib,
  pkgs,
  ...
}:
{
  config.lib.stylix.imageEditors = {
    lutgen = import ./lutgen.nix { inherit lib config pkgs; };
  };
}
