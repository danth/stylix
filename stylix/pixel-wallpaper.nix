{ pkgs, config, ... }:

{
  # Generate a PNG image containing a named color
  color:
    pkgs.runCommand "${color}-pixel.png" {
      color = config.lib.stylix.colors.withHashtag.${color};
    } "${pkgs.imagemagick}/bin/convert xc:$color png32:$out";
}
