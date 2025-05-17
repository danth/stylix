{ lib, pkgs, ... }:
{
  stylix.testbed.ui.command = {
    text = lib.getExe pkgs.bashInteractive;
    useTerminal = true;
  };

  home-manager.sharedModules = lib.singleton {
    programs = {
      starship = {
        enable = true;
        package = pkgs.starship;
        enableBashIntegration = true;
      };
      bash = {
        enable = true;
        package = pkgs.bashInteractive;
      };
    };
  };
}
