{ lib, pkgs, ... }:

let
  package = pkgs.foot;
in
{
  stylix.testbed.ui.application = {
    name = "foot";
    inherit package;
  };

  home-manager.sharedModules = lib.singleton {
    programs.foot = {
      enable = true;
      inherit package;
    };
  };
}
