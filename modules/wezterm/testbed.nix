{ lib, pkgs, ... }:

let
  package = pkgs.wezterm;
in
{
  stylix.testbed.application = {
    enable = true;
    name = "org.wezfurlong.wezterm";
    inherit package;
  };

  home-manager.sharedModules = lib.singleton {
    programs.wezterm = {
      enable = true;
      inherit package;
    };
  };
}
