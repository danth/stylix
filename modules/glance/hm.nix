{ mkTarget, lib, ... }:
mkTarget {
  name = "glance";
  humanName = "Glance";

  configElements = [
    (
      { polarity }:
      {
        services.glance.settings.theme.light = polarity == "light";
      }
    )
    (
      { colors }:
      let
        rgb-to-hsl = import ./rgb-to-hsl.nix { inherit lib colors; };
      in
      {
        services.glance.settings.theme = {
          contrast-multiplier = 1.0;
          background-color = rgb-to-hsl "base00";
          primary-color = rgb-to-hsl "base05";
          positive-color = rgb-to-hsl "base01";
          negative-color = rgb-to-hsl "base04";
        };
      }
    )
  ];
}
