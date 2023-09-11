{ config, lib, pkgs, ... }:

with config.lib.stylix.colors;

let
  rgb = color: "rgb(${color})";
  rgba = color: alpha: "rgba(${color}${alpha})";

  settings = {
    exec-once = if !(config.lib.stylix.isVideo config.stylix.wallpaper) then [      
      "${pkgs.swww}/bin/swww-daemon" 
    ] else [];
    exec = if (config.lib.stylix.isVideo config.stylix.wallpaper) then [
      "${pkgs.mpvpaper}/bin/mpvpaper '*' -o 'no-audio --loop' ${config.stylix.wallpaper.video}"              
    ] else if (config.lib.stylix.isSlideshow config.stylix.wallpaper) then [
      "${config.lib.stylix.waylandSlideshowScript}"
    ] else [
      "${pkgs.swww}/bin/swww img ${config.stylix.wallpaper.animation}"
    ];
    decoration."col.shadow" = rgba base00 "99";
    general = {
      "col.active_border" = rgb base0A;
      "col.inactive_border" = rgb base03;
      "col.group_border" = rgb base0D;
      "col.group_border_active" = rgb base06;
      "col.group_border_locked_active" = rgb base06;
    };
    misc.background_color = rgb base00;
  };

in {
  options.stylix.targets.hyprland.enable =
    config.lib.stylix.mkEnableTarget "Hyprland" true;

  config.wayland.windowManager.hyprland.settings =
    lib.mkIf config.stylix.targets.hyprland.enable settings;
}
