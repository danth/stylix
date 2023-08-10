{ config, lib, ... }:

with config.lib.stylix.colors;

let
  text = base05;
  urgent = base08;
  focused = base0A;
  unfocused = base03;

  fonts = {
    names = [ config.stylix.fonts.sansSerif.name ];
    size = config.stylix.fonts.sizes.desktop + 0.0;
  };

in {
    options.stylix.targets.hyprland.enable = 
      config.lib.stylix.mkEnableTarget "Hyprland" true;
    config = lib.mkMerge [
      (lib.mkIf config.stylix.targets.hyprland.enable {
        wayland.windowManager.hyprland.settings = {
          general = {
            "col.active_border" = "rgb(${focused})";
            "col.inactive_border" = "rgb(${unfocused})";
            # "col.active_border" = "rgb(${focused})";
          };
        };
      })
    ];
  }
