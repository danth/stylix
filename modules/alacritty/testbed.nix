{ lib, pkgs, ... }:

let
  package = pkgs.alacritty;
in
{
  stylix.testbed.application = {
    enable = true;
    name = "Alacritty";
    inherit package;
  };

  home-manager.sharedModules = lib.singleton {
    programs.alacritty = {
      enable = true;
      inherit package;
    };
  };
}
