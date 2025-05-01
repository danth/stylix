{ lib, pkgs, ... }:

{
  # We use Hyprland because Gnome has its own notification daemon
  environment.loginShellInit = lib.getExe pkgs.hyprland;
  programs.hyprland.enable = true;

  home-manager.sharedModules = lib.singleton {
    services.mako.enable = true;
    wayland.windowManager.hyprland = {
      enable = true;
      settings.exec-once = [
        "${lib.getExe pkgs.libnotify} -u low Low Urgency"
        "${lib.getExe pkgs.libnotify} -u normal Normal Urgency"
        "${lib.getExe pkgs.libnotify} -u critical Critical Urgency"
      ];
    };
  };
}
