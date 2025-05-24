{ mkTarget, ... }:
mkTarget {
  name = "fcitx5";
  humanName = "fcitx5";

  configElements = [
    (
      { fonts }:
      {
        i18n.inputMethod.fcitx5.settings.addons.classicui.globalSection = with fonts; {
          Font = "${sansSerif.name} ${toString sizes.popups}";
          MenuFont = "${sansSerif.name} ${toString sizes.popups}";
          TrayFont = "${sansSerif.name} ${toString sizes.popups}";
        };
      }
    )
    (
      { colors }:
      {
        i18n.inputMethod.fcitx5 = {
          settings.addons.classicui.globalSection = {
            Theme = "stylix";
            UseDarkTheme = false;
            UseAccentColor = false;
          };
          themes.stylix = {
            # Adapted from https://github.com/sanweiya/fcitx5-mellow-themes under the BSD 2 license (compatible with this project's license (MIT))
            # Copyright (c) 2024, sanweiya
            highlightImage = colors {
              template = ./highlight.svg.mustache;
              extension = ".svg";
            };
            panelImage = colors {
              template = ./panel.svg.mustache;
              extension = ".svg";
            };
            theme = import ./template.nix {
              colors = colors.withHashtag;
            };
          };
        };
      }
    )
  ];
}
