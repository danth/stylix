{
  config,
  lib,
  ...
}:
{
  options.stylix.targets.fcitx5.enable =
    config.lib.stylix.mkEnableTarget "fcitx5" true;

  config =
    lib.mkIf (config.stylix.enable && config.stylix.targets.fcitx5.enable)
      {
        i18n.inputMethod.fcitx5 = {
          settings.addons.classicui.globalSection = with config.stylix.fonts; {
            Theme = "stylix";
            UseDarkTheme = false;
            UseAccentColor = false;

            Font = "${sansSerif.name} ${toString sizes.popups}";
            MenuFont = "${sansSerif.name} ${toString sizes.popups}";
            TrayFont = "${sansSerif.name} ${toString sizes.popups}";
          };
          themes.stylix = {
            # Adapted from https://github.com/sanweiya/fcitx5-mellow-themes under the BSD 2 license (compatible with this project's license (MIT))
            # Copyright (c) 2024, sanweiya
            highlightImage = config.lib.stylix.colors {
              template = ./highlight.svg.mustache;
              extension = ".svg";
            };
            panelImage = config.lib.stylix.colors {
              template = ./panel.svg.mustache;
              extension = ".svg";
            };
            theme = import ./template.nix {
              colors = config.lib.stylix.colors.withHashtag;
            };
          };
        };
      };
}
