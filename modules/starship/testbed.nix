{ lib, pkgs, ... }:
{
  stylix.testbed.application = {
    enable = true;
    name = "Alacritty";
    package = pkgs.alacritty;
  };

  home-manager.sharedModules = lib.singleton {
    programs = {
      alacritty = {
        enable = true;
        package = pkgs.alacritty;
      };
      bash = {
        enable = true;
        package = pkgs.bashInteractive;
      };
      starship = {
        enable = true;
        package = pkgs.starship;
        enableBashIntegration = true;
      };
    };
  };
}
