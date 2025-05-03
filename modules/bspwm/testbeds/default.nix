{ lib, pkgs, ... }:

{
  config.stylix.testbed.ui = {
    desktop = "bspwm";
    # We need something to open a window so that we can check the window borders
    command.text = lib.getExe pkgs.kitty;
  };
  config.services.displayManager.autoLogin.enable = false;
}
