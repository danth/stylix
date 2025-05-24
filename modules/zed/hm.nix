{ mkTarget, ... }:
mkTarget {
  name = "zed";
  humanName = "zed";

  configElements = [
    (
      { fonts }:
      {
        programs.zed-editor = {
          userSettings = {
            "buffer_font_family" = fonts.monospace.name;
            "buffer_font_size" = fonts.sizes.terminal * 4.0 / 3.0;
            "ui_font_family" = fonts.sansSerif.name;
            "ui_font_size" = fonts.sizes.applications * 4.0 / 3.0;
          };
        };
      }
    )
    (
      { colors, inputs }:
      {
        programs.zed-editor = {
          userSettings.theme = "Base16 ${colors.scheme-name}";
          themes.stylix = colors {
            templateRepo = inputs.tinted-zed;
          };
        };
      }
    )
  ];
}
