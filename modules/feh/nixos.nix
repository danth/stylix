{
  pkgs,
  config,
  lib,
  ...
}:
let
  cfg = config.stylix.targets.feh;
in
{
  options.stylix.targets.feh = {
    enable = config.lib.stylix.mkEnableTarget "the desktop background using Feh" true;
    useWallpaper = config.lib.stylix.mkEnableWallpaper "Feh" true;
  };

  config.services.xserver.displayManager.sessionCommands =
    lib.mkIf
      (
        config.stylix.enable
        && cfg.enable
        && (with config.services.xserver.windowManager; xmonad.enable || i3.enable)
      )
      (
        let
          inherit (config.stylix) imageScalingMode;
          bg-arg =
            if imageScalingMode == "fill" then
              "--bg-fill"
            else if imageScalingMode == "center" then
              "--bg-center"
            else if imageScalingMode == "tile" then
              "--bg-tile"
            else if imageScalingMode == "stretch" then
              "--bg-scale"
            # Fit
            else
              "--bg-max";
        in
        lib.optionalString cfg.useWallpaper "${pkgs.feh}/bin/feh --no-fehbg ${bg-arg} ${config.stylix.image}"
      );
}
