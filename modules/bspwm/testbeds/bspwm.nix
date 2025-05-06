{ lib, pkgs, ... }:

{
  services.xserver = {
    enable = true;
    windowManager.bspwm.enable = true;
  };

  home-manager.sharedModules = lib.singleton {
    xsession.windowManager.bspwm = {
      enable = true;

      # We need something to open a window so that we can check the window borders
      startupPrograms = [ "${lib.getExe pkgs.kitty}" ];
    };
  };
}
