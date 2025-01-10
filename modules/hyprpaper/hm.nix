{ config, lib, ... }:
{
  options.stylix.targets.hyprpaper.enable =
    config.lib.stylix.mkEnableTarget "Hyprpaper" true;

  config =
    lib.mkIf (config.stylix.enable && config.stylix.targets.hyprpaper.enable)
      {
        services.hyprpaper.settings = {
          preload = [ "${config.stylix.image}" ];
          wallpaper = [ ",${config.stylix.image}" ];
        };
      };
}
