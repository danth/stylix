{ lib, pkgs, ... }:
let
  package = pkgs.neovide;
in
{
  stylix.testbed.ui.application = {
    name = "neovide";
    inherit package;
  };

  home-manager.sharedModules = lib.singleton {
    programs = {
      neovide = {
        enable = true;
        inherit package;
      };
      neovim.enable = true;
    };
  };
}
