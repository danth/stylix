{ pkgs, config, lib, ... }:

with config.lib.stylix;
let
  slideshowScript = pkgs.writeScript "script.sh" ''
    # This controls (in seconds) when to switch to the next image
    INTERVAL=${builtins.toString config.stylix.wallpaper.delay}
    imagearray=(${builtins.toString config.stylix.wallpaper.images})
    
    while true; do
    	${pkgs.feh}/bin/feh --no-fehbg --bg-scale ''${imagearray[ $RANDOM % ''${#imagearray[@]} ]}
    	sleep $INTERVAL
    done
  '';
in {
  options.stylix.targets.xorg.enable =
    config.lib.stylix.mkEnableTarget
      "the desktop background of an xorg session"
      (with config.services.xserver.windowManager; xmonad.enable || i3.enable);

  config.services.xserver.displayManager.sessionCommands = lib.mkIf config.stylix.targets.xorg.enable 
    (if (isAnimation config.stylix.wallpaper) then ''
      ${pkgs.xwinwrap}/bin/xwinwrap -fs -ni -b -nf -ov -- mpv -wid WID --loop --no-audio ${config.stylix.wallpaper.animation}
    '' else
      (if (isVideo config.stylix.wallpaper) then ''
        ${pkgs.xwinwrap}/bin/xwinwrap -fs -ni -b -nf -ov -- mpv -wid WID --loop --no-audio ${config.stylix.wallpaper.video}
      '' else
        (if (isSlideshow config.stylix.wallpaper) then "${slideshowScript}"
        else "${pkgs.feh}/bin/feh --no-fehbg --bg-scale ${config.stylix.wallpaper.image}")));
}
