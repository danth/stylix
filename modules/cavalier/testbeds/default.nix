{ lib, pkgs, ... }:

let
  package = pkgs.cavalier;
in
{
  stylix.testbed.application = {
    enable = true;
    name = "org.nickvision.cavalier";
    inherit package;
  };

  home-manager.sharedModules = lib.singleton {
    programs.cavalier = {
      enable = true;
      inherit package;
    };
  };
}
