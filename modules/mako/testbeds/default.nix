{ lib, pkgs, ... }:

{
  # We use Hyprland because Gnome has its own notification daemon
  environment.loginShellInit = lib.getExe pkgs.hyprland;
  programs.hyprland.enable = true;

  home-manager.sharedModules = lib.singleton {
    services.mako.enable = true;
    wayland.windowManager.hyprland = {
      enable = true;
      settings.exec-once =
        # Run as a single command to ensure the same order between executions
        lib.concatMapStringsSep " && "
          (
            urgency: "${lib.getExe pkgs.libnotify} --urgency ${urgency} ${urgency} urgency"
          )
          [
            "low"
            "normal"
            "critical"
          ];
    };
  };
}
