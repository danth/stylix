{ config, lib, ... }:

with config.lib.stylix.colors;

let
  rgb = color: "rgb(${color})";
  rgba = color: alpha: "rgba(${color}${alpha})";

  settings = {
    decoration."col.shadow" = rgba base00 "99";
    general = {
      "col.active_border" = rgb base0D;
      "col.inactive_border" = rgb base03;
    };
    group = {
      "col.border_inactive" = rgb base03;
      "col.border_active" = rgb base0D;
      "col.border_locked_active" = rgb base0C;

      groupbar = {
        text_color = rgb base05;
        "col.active" = rgb base0D;
        "col.inactive" = rgb base03;
      };
    };
    misc.background_color = rgb base00;
  };

in {
  options.stylix.targets.hyprland.enable =
    config.lib.stylix.mkEnableTarget "Hyprland" true;

  config =
    lib.mkIf
    (config.stylix.enable && config.stylix.targets.hyprland.enable && config.wayland.windowManager.hyprland.enable)
    {
      services.hyprpaper.enable = true;
      stylix.targets.hyprpaper.enable = true;
      wayland.windowManager.hyprland.settings = settings;
    };
}
