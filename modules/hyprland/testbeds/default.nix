{ lib, pkgs, ... }:

{
  stylix.testbed.ui.graphicalEnvironment = "hyprland";

  home-manager.sharedModules = lib.singleton {
    # We need something to open a window so that we can check the window borders
    wayland.windowManager.hyprland.settings.bind = [
      "ALT, RETURN, exec, ${lib.getExe pkgs.foot}"
    ];
  };
}
