# Dummy values to avoid errors when generating the documentation.

{ lib, ... }:
{
  stylix = {
    image = null;

    base16Scheme = lib.genAttrs [
      "base00"
      "base01"
      "base02"
      "base03"
      "base04"
      "base05"
      "base06"
      "base07"
      "base08"
      "base09"
      "base0A"
      "base0B"
      "base0C"
      "base0D"
      "base0E"
      "base0F"
    ] (_: "ffffff");
  };
}
