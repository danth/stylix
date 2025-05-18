{ lib, pkgs, ... }:
let
  package = pkgs.mpv;
in
{
  stylix.testbed.ui.application = {
    name = "mpv";
    inherit package;
  };

  home-manager.sharedModules = lib.singleton {
    programs.mpv = {
      enable = true;
      inherit package;
      scripts = [ pkgs.mpvScripts.uosc ];
    };
  };
}
