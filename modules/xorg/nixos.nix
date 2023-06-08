{ pkgs, config, lib, ... }:

{
  options.stylix.targets.xorg.enable =
    config.lib.stylix.mkEnableTarget
    "the desktop background of an xorg session"
    (with config.services.xserver.windowManager; xmonad.enable || i3.enable);

  config.services.xserver.displayManager.sessionCommands = lib.mkIf config.stylix.targets.xorg.enable
  (if (config.lib.stylix.isAnimation config.stylix.wallpaper) then ''
      ${pkgs.xwinwrap}/bin/xwinwrap -fs -ni -b -nf -ov -- mpv -wid WID --loop --no-audio ${config.stylix.wallpaper.animation}
    '' else (if (config.lib.stylix.isVideo config.stylix.wallpaper) then ''
    '' else "${pkgs.feh}/bin/feh --no-fehbg --bg-scale ${config.stylix.wallpaper.image}"));
}
