{ pkgs, config, ... }:

with config.lib.stylix;

let
  inherit (config.stylix) wallpaper;

  fehCommand = file: ''
    ${pkgs.feh}/bin/feh --no-fehbg --bg-scale ${file}
  '';

  mpvCommand = file: ''
    ${pkgs.xwinwrap}/bin/xwinwrap -fs -ni -b -nf -ov -- \
      ${pkgs.mpv}/bin/mpv -wid WID --loop --no-audio ${file}
  '';

  slideshow = pkgs.writeShellApplication {
    name = "slideshow";
    runtimeInputs = [ pkgs.feh ];
    text = ''
      images=(${toString wallpaper.images})
      while true; do
        feh --no-fehbg --bg-scale ''${images[ $RANDOM % ''${#images[@]} ]}
        sleep ${toString wallpaper.delay}
      done
   '';
  };

in
  if isVideo wallpaper
  then mpvCommand wallpaper.video
  else if isAnimation wallpaper
  then mpvCommand wallpaper.animation
  else if isSlideshow wallpaper
  then "${slideshow}/bin/slideshow"
  else fehCommand wallpaper.image
