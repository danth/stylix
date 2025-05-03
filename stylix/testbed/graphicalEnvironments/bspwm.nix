{
  config,
  lib,
  pkgs,
  ...
}:

{
  config =
    lib.mkIf (config.stylix.testbed.ui.graphicalEnvironment or null == "bspwm")
      {
        services.xserver = {
          enable = true;
          windowManager.bspwm.enable = true;
        };

        home-manager.sharedModules = lib.singleton {
          xsession.windowManager.bspwm = {
            enable = true;
            startupPrograms = [
              "find /run/current-system/sw/etc/xdg/autostart/ -type f -or -type l | xargs -P0 -L1 ${lib.getExe pkgs.dex}"
            ];
          };
        };
      };
}
