{
  pkgs,
  config,
  lib,
  ...
}:
{
  # Generate a PNG image containing a named color
  config.lib.stylix.pixel =
    color:
    pkgs.runCommand "${color}-pixel.png" {
      color = config.lib.stylix.colors.withHashtag.${color};
    } "${lib.getExe' pkgs.imagemagick "convert"} xc:$color png32:$out";
}
