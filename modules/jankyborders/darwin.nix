{ config, lib, ... }:
let
  inherit (config.lib.stylix) colors mkOpacityHexColor;
in
{
  options.stylix.targets.jankyborders.enable =
    config.lib.stylix.mkEnableTarget "JankyBorders" true;

  config =
    lib.mkIf (config.stylix.enable && config.stylix.targets.jankyborders.enable)
      {
        services.jankyborders = {
          active_color = mkOpacityHexColor colors.base0D config.stylix.opacity.desktop;
          inactive_color = mkOpacityHexColor colors.base03 config.stylix.opacity.desktop;
        };
      };
}
