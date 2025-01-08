{ lib, pkgs, ... }:

let
  package = pkgs.foot;
in
{
  stylix.testbed.application = {
    enable = true;
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
