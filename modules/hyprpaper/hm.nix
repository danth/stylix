{ config, lib, ... }:
let
  cfg = config.stylix.targets.hyprpaper;
in
{
  options.stylix.targets.hyprpaper = {
    enable = config.lib.stylix.mkEnableTarget "Hyprpaper" true;
    useWallpaper = config.lib.stylix.mkEnableWallpaper "Hyprpaper" true;
  };

  config = lib.mkIf (config.stylix.enable && cfg.enable) {
    services.hyprpaper.settings = lib.mkIf cfg.useWallpaper {
      preload = [ "${config.stylix.image}" ];
      wallpaper = [ ",${config.stylix.image}" ];
    };
  };
}
