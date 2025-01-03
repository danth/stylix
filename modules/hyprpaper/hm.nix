{ config, lib, ... }: {
  options.stylix.targets.hyprpaper = {
    enable = config.lib.stylix.mkEnableTarget "Hyprpaper" true;
    wallpaper = config.lib.stylix.mkEnableWallpaper "Hyprpaper";
  };

  config =
    lib.mkIf
    (config.stylix.enable && config.stylix.targets.hyprpaper.enable && config.stylix.targets.hyprpaper.wallpaper)
    {
      services.hyprpaper.settings = {
        preload = [ "${config.stylix.image}" ];
        wallpaper = [ ",${config.stylix.image}" ];
      };
    };
}
