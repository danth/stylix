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
          # settings.addons.classicui.globalSection.Theme = "stylix";
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
            theme = import ./theme.nix { colors = config.lib.stylix.colors.withHashtag; };
          };
        };
      };
}
