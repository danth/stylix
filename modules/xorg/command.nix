{ pkgs, config, ... }:

config.stylix.wallpaper.unpack rec {
  image =
    { file, ... }:
    "${pkgs.feh}/bin/feh --no-fehbg --bg-scale ${file}";

  slideshow =
    { files, delay, ... }:
    let slideshow = pkgs.writeShellApplication {
      name = "slideshow";
      runtimeInputs = [ pkgs.feh ];
      text = ''
        images=(${toString (map lib.escapeShellArg files)})
        while true; do
          feh --no-fehbg --bg-scale ''${images[ $RANDOM % ''${#images[@]} ]}
          sleep ${toString delay}
        done
     '';
    };
    in "${slideshow}/bin/slideshow";

  animation = video;

  video = { file, ... }: ''
    ${pkgs.xwinwrap}/bin/xwinwrap -fs -ni -b -nf -ov -- \
      ${pkgs.mpv}/bin/mpv -wid WID --loop --no-audio ${file}
  '';
}
