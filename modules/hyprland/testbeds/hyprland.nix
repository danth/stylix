{ lib, pkgs, ... }:

{
  environment.loginShellInit = lib.getExe pkgs.hyprland;
  programs.hyprland.enable = true;

  home-manager.sharedModules = lib.singleton {
    wayland.windowManager.hyprland = {
      enable = true;

      # We need something to open a window so that we can check the window borders
      settings.bind = [ "ALT, RETURN, exec, ${lib.getExe pkgs.foot}" ];
    };
  };
}
