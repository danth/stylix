{ mkTarget, lib, ... }:
mkTarget {
  name = "fnott";
  humanName = "Fnott";

  configElements = [
    (
      { fonts }:
      {
        services.fnott.settings =
          let
            font = "${fonts.sansSerif.name}:size=${toString fonts.sizes.popups}";
          in
          {
            main = {
              title-font = font;
              summary-font = font;
              body-font = font;
            };
          };
      }
    )
    (
      { colors, opacity }:
      {
        services.fnott.settings =
          let
            fg = c: "${c}ff";
            bg =
              c:
              "${c}${
                lib.toHexString (((builtins.floor (opacity.popups * 100 + 0.5)) * 255) / 100)
              }";
          in
          with colors;
          {
            main = {
              title-color = fg base05;
              summary-color = fg base05;
              body-color = fg base05;
              progress-bar-color = fg base02;
              background = bg base00;
            };

            low.border-color = fg base03;
            normal.border-color = fg base0D;
            critical.border-color = fg base08;
          };
      }
    )
  ];
}
