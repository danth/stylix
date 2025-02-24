{ config, lib, ... }:
let
  cfg = config.stylix.targets.hyprpaper;
in
{
  options.stylix.targets.hyprpaper = {
    enable = config.lib.stylix.mkEnableTarget "Hyprpaper" (
      config.stylix.image != null
    );
  };

  config = lib.mkIf (config.stylix.enable && cfg.enable) {
    services.hyprpaper.settings = {
      preload = [ "${config.stylix.image}" ];
      wallpaper = [ ",${config.stylix.image}" ];
    };
  };
}
