{
  mkTarget,
  config,
  lib,
  ...
}:
mkTarget {
  name = "lightdm";
  humanName = "LightDM";

  extraOptions = {
    useWallpaper = config.lib.stylix.mkEnableWallpaper "LightDM" true;
  };

  configElements =
    { cfg, image }:
    {
      services.xserver.displayManager.lightdm.background =
        lib.mkIf cfg.useWallpaper image;
    };
}
