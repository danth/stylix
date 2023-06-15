{ pkgs, config, lib, ... }:

with config.lib.stylix;
let
  slideshowScript = pkgs.writeScript "script.sh" ''
    if [[ $# -lt 1 ]] || [[ ! -d $1 ]]; then
      echo "Usage:
      $0 <dir containg images>"
      exit 1
    fi

    export SWWW_TRANSITION_FPS=60
    export SWWW_TRANSITION_STEP=2

    # This controls (in seconds) when to switch to the next image
    INTERVAL=${builtins.toString config.stylix.wallpaper.delay}
    
    while true; do
    	find "$1" \
    		| while read -r img; do
    			echo "$((RANDOM % 1000)):$img"
    		done \
    		| sort -n | cut -d':' -f2- \
    		| while read -r img; do
    			${pkgs.feh}/bin/feh --no-fehbg --bg-scale "$img"
    			sleep $INTERVAL
    		done
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
        (if (isSlideshow config.stylix.wallpaper) then "${slideshowScript} ${config.stylix.wallpaper.images}"
        else "${pkgs.feh}/bin/feh --no-fehbg --bg-scale ${config.stylix.wallpaper.image}")));
}
