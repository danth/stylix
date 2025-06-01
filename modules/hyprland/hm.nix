{
  config,
  lib,
  mkTarget,
  ...
}:
mkTarget {
  name = "hyprland";
  humanName = "Hyprland";
  extraOptions.hyprpaper.enable = config.lib.stylix.mkEnableTarget "Hyprpaper" (
    config.stylix.image != null
  );
  configElements = [
    (
      { colors }:
      {
        wayland.windowManager.hyprland.settings =
          let
            rgb = color: "rgb(${color})";
            rgba = color: alpha: "rgba(${color}${alpha})";
          in
          {
            decoration.shadow.color = rgba colors.base00 "99";
            general = {
              "col.active_border" = rgb colors.base0D;
              "col.inactive_border" = rgb colors.base03;
            };
            group = {
              "col.border_inactive" = rgb colors.base03;
              "col.border_active" = rgb colors.base0D;
              "col.border_locked_active" = rgb colors.base0C;

              groupbar = {
                text_color = rgb colors.base05;
                "col.active" = rgb colors.base0D;
                "col.inactive" = rgb colors.base03;
              };
            };
            misc.background_color = rgb colors.base00;
          };
      }
    )
    (
      { cfg }:
      lib.mkIf (config.wayland.windowManager.hyprland.enable && cfg.hyprpaper.enable)
        {
          services.hyprpaper.enable = true;
          stylix.targets.hyprpaper.enable = true;
          wayland.windowManager.hyprland.settings.misc.disable_hyprland_logo = true;
        }
    )
  ];
}
