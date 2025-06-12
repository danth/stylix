{
  mkTarget,
  lib,
  pkgs,
  config,
  ...
}:
mkTarget {
  name = "feh";
  humanName = "the desktop background using Feh";
  autoEnable =
    with config.services.xserver.windowManager;
    xmonad.enable || i3.enable;
  autoEnableExpr = ''
    with services.xserver.windowManager;
    xmonad.enable || i3.enable
  '';

  configElements =
    { image, imageScalingMode }:
    {
      services.xserver.displayManager.sessionCommands =
        let
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
        "${lib.getExe pkgs.feh} --no-fehbg ${bg-arg} ${image}";
    };
}
