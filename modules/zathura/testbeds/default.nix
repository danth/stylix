{ lib, pkgs, ... }:

let
  package = pkgs.zathura;
in
{
  stylix.testbed.ui.application = {
    name = "org.pwmt.zathura";
    inherit package;
  };

  home-manager.sharedModules = lib.singleton {
    programs.zathura = {
      enable = true;
      inherit package;
    };
  };
}
