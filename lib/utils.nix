{ config, lib, pkgs, ... }:

{
  config.lib.stylix = {
    # get the opacity values as strings in hexadecimal, integer and floating point values

    opacityToHex = opacity: lib.toHexString (
      builtins.ceil (opacity * 255)
    );

    opacityToInt = opacity: builtins.toString (
      builtins.ceil (opacity * 100));

    opacityToFloat = opacity: builtins.toString
      opacity;

    polarityFrom = colors:
      let
        red = lib.toInt colors.base00-rgb-r;
        green = lib.toInt colors.base00-rgb-g;
        blue = lib.toInt colors.base00-rgb-b;
      in
      if
        (red + green + blue) >= 150
      then "light"
      else "dark";

    # Generate a PNG image containing a named color

    solid = color: pkgs.runCommand "${color}-pixel.png" { } "${pkgs.imagemagick}/bin/convert xc:#${color} png32:$out";
    pixel = color:
      pkgs.runCommand "${color}-pixel.png"
        {
          color = config.stylix.colors.withHashtag.${color};
        } "${pkgs.imagemagick}/bin/convert xc:$color png32:$out";
  };
}
