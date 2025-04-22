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
          classicUiConfig = "Theme=stylix";
          themes.stylix = {
            highlightImage = config.lib.stylix.colors {
              template = ./highlight.svg.mustache;
              extension = ".svg";
            };
            panelImage = config.lib.stylix.colors {
              template = ./panel.svg.mustache;
              extension = ".svg";
            };
            theme = config.lib.stylix.colors {
              template = ./theme.conf.mustache;
              extension = ".conf";
            };
          };
        };
      };
}
