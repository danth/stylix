{ config, lib, ... }:
{
  options.stylix.targets.glance.enable =
    config.lib.stylix.mkEnableTarget "Glance" true;

  config =
    lib.mkIf (config.stylix.enable && config.stylix.targets.glance.enable)
      {
        services.glance.settings.theme =
          let
            rgb-to-hsl =
              color:
              let
                r = ((lib.toInt config.lib.stylix.colors."${color}-rgb-r") * 100) / 255;
                g = ((lib.toInt config.lib.stylix.colors."${color}-rgb-g") * 100) / 255;
                b = ((lib.toInt config.lib.stylix.colors."${color}-rgb-b") * 100) / 255;
                max = lib.max r (lib.max g b);
                min = lib.min r (lib.min g b);
                delta = max - min;
                h =
                  if delta == 0 then
                    0
                  else if max == r then
                    60 * (lib.mod ((g - b) / delta) 6)
                  else if max == g then
                    60 * (((b - r) / delta) + 2)
                  else if max == b then
                    60 * (((r - g) / delta) + 4)
                  else
                    0;
                l = (max + min) / 2;
                s =
                  if delta == 0 then 0 else delta / (100 - lib.max (2 * l - 100) (100 - (2 * l)));
              in
              "${builtins.toString h} ${builtins.toString s} ${builtins.toString l}";
          in
          {
            light = config.stylix.polarity == "light";
            contrast-multiplier = 1.0;
            background-color = rgb-to-hsl "base00";
            primary-color = rgb-to-hsl "base05";
            positive-color = rgb-to-hsl "base01";
            negative-color = rgb-to-hsl "base04";
          };
      };
}
