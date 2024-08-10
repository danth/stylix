{ config, lib, ... }:

{
  options.stylix.targets.hyprland = {
    enable = config.lib.stylix.mkEnableTarget "Hyprland" true;
    hyprpaper.enable = config.lib.stylix.mkEnableTarget "Hyprpaper" true;
  };

  config = let
    cfg = config.stylix.targets.hyprland;
  in
    lib.mkIf
    (
      config.stylix.enable
      && cfg.enable
      && config.wayland.windowManager.hyprland.enable
    )
    (
      lib.mkMerge [
        {
          wayland.windowManager.hyprland.settings = let
            rgb = color: "rgb(${color})";
            rgba = color: alpha: "rgba(${color}${alpha})";
          in with config.lib.stylix.colors; {
            decoration.shadow.color = rgba base00 "99";
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
        }

        (
          lib.mkIf cfg.hyprpaper.enable {
            services.hyprpaper.enable = true;
            stylix.targets.hyprpaper.enable = true;
          }
        )
      ]
    );
}
