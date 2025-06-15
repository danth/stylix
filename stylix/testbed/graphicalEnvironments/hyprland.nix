{
  config,
  lib,
  pkgs,
  ...
}:

{
  config =
    lib.mkIf (config.stylix.testbed.ui.graphicalEnvironment or null == "hyprland")
      {
        environment.loginShellInit = lib.getExe pkgs.hyprland;
        programs.hyprland.enable = true;
        home-manager.sharedModules = lib.singleton {
          wayland.windowManager.hyprland = {
            enable = true;
            settings.exec-once = "find /run/current-system/sw/etc/xdg/autostart/ -type f -or -type l | xargs -P0 -L1 ${lib.getExe pkgs.dex}";
          };
        };
      };
}
