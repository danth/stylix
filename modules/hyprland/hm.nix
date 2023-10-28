{ config, lib, pkgs, ... }:

with config.stylix.colors;

let
  rgb = color: "rgb(${color})";
  rgba = color: alpha: "rgba(${color}${alpha})";

in {
  options.stylix.targets.hyprland.enable =
    config.lib.stylix.mkEnableTarget "Hyprland"
    config.wayland.windowManager.hyprland.enable;

  config = lib.mkIf config.stylix.targets.hyprland.enable {
    stylix.targets.wlroots.enable = true;

    wayland.windowManager.hyprland.settings = {
      decoration."col.shadow" = rgba base00 "99";
      general = {
        "col.active_border" = rgb base0A;
        "col.inactive_border" = rgb base03;
      };
      group = {
        "col.border_inactive" = rgb base0D;
        "col.border_active" = rgb base06;
        "col.border_locked_active" = rgb base06;
      };
      misc.background_color = rgb base00;
    };
  };
}
