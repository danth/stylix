{ lib, config, ... }:
{
  options.stylix.targets.fnott.enable =
    config.lib.stylix.mkEnableTarget "Fnott" true;

  config.services.fnott.settings =
    lib.mkIf (config.stylix.enable && config.stylix.targets.fnott.enable)
      (
        let
          font = config.stylix.fonts.sansSerif.name;
          fg = c: "${c}ff";
          bg =
            c:
            "${c}${
              lib.toHexString (
                ((builtins.floor (config.stylix.opacity.popups * 100 + 0.5)) * 255) / 100
              )
            }";
        in
        with config.lib.stylix.colors;
        {
          main = {
            title-font = font;
            summary-font = font;
            body-font = font;

            title-color = fg base05;
            summary-color = fg base05;
            body-color = fg base05;
            progress-bar-color = fg base02;
            background = bg base00;
          };

          low.border-color = fg base0B;
          normal.border-color = fg base0E;
          critical.border-color = fg base08;
        }
      );
}
