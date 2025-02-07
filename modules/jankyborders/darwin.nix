{ config, lib, ... }:
let
  inherit (config.lib.stylix) colors;

  opacityHex =
    percentage:
    lib.throwIfNot (percentage >= 0 && percentage <= 1)
      "value must be between 0 and 1 (inclusive): ${toString percentage}"
      (lib.toHexString (builtins.floor (percentage * 255 + 0.5)));
  mkColor =
    color:
    "0x${opacityHex config.stylix.opacity.desktop}${lib.removePrefix "#" color}";
in
{
  options.stylix.targets.jankyborders.enable =
    config.lib.stylix.mkEnableTarget "JankyBorders" true;

  config =
    lib.mkIf (config.stylix.enable && config.stylix.targets.jankyborders.enable)
      {
        services.jankyborders = {
          active_color = mkColor colors.base0D;
          inactive_color = mkColor colors.base03;
        };
      };
}
