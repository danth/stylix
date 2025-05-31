{ lib, ... }:
let
  opacityHex =
    percentage:
    lib.throwIfNot (percentage >= 0 && percentage <= 1)
      "value must be between 0 and 1 (inclusive): ${toString percentage}"
      (lib.toHexString (builtins.floor (percentage * 255 + 0.5)));
in
{
  config.lib.stylix = {
    mkOpacityHexColor =
      color: opacity: "0x${opacityHex opacity}${lib.removePrefix "#" color}";

    mkHexColor = color: "0x${lib.removePrefix "#" color}";
  };
}
