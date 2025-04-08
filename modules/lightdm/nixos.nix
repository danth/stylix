{ config, lib, ... }:
let
  cfg = config.stylix.targets.lightdm;
in
{
  options.stylix.targets.lightdm = {
    enable = config.lib.stylix.mkEnableTarget "LightDM" true;
    useWallpaper = config.lib.stylix.mkEnableWallpaper "LightDM" true;
  };

  config.services.xserver.displayManager.lightdm.background = lib.mkIf (
    config.stylix.enable && cfg.enable && cfg.useWallpaper
  ) config.stylix.image;
}
