{ config, lib, ... }:

let
  rgb-to-hsl = import ./rgb-to-hsl.nix { inherit lib config; };
in
{
  options.stylix.targets.glance.enable =
    config.lib.stylix.mkEnableTarget "Glance" true;

  config =
    lib.mkIf (config.stylix.enable && config.stylix.targets.glance.enable)
      {
        services.glance.settings.theme = {
          light = config.stylix.polarity == "light";
          contrast-multiplier = 1.0;
          background-color = rgb-to-hsl "base00";
          primary-color = rgb-to-hsl "base05";
          positive-color = rgb-to-hsl "base01";
          negative-color = rgb-to-hsl "base04";
        };
      };
}
