{
  pkgs,
  config,
  lib,
  ...
}:

{
  options.stylix.targets.feh.enable =
    config.lib.stylix.mkEnableTarget "the desktop background using Feh" true;

  config.xsession.initExtra =
    lib.mkIf
      (
        config.stylix.enable
        && config.stylix.targets.feh.enable
        && (
          with config.xsession.windowManager;
          bspwm.enable
          || herbstluftwm.enable
          || i3.enable
          || spectrwm.enable
          || xmonad.enable
        )
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
        "${pkgs.feh}/bin/feh --no-fehbg ${bg-arg} ${config.stylix.image}"
      );
}
