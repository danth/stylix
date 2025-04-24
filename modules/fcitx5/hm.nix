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
        xdg = {
          configFile."fcitx5/conf/classicui.conf".text = "Theme=stylix";
          dataFile = {
            # Adapted from https://github.com/sanweiya/fcitx5-mellow-themes under the BSD 2 license (compatible with this project's license (MIT))
            # Copyright (c) 2024, sanweiya
            "fcitx5/themes/stylix/highlight.svg".source = config.lib.stylix.colors {
              template = ./highlight.svg.mustache;
              extension = ".svg";
            };
            "fcitx5/themes/stylix/panel.svg".source = config.lib.stylix.colors {
              template = ./panel.svg.mustache;
              extension = ".svg";
            };
            "fcitx5/themes/stylix/theme.conf".source = config.lib.stylix.colors {
              template = ./theme.conf.mustache;
              extension = ".conf";
            };
          };
        };
      };
}
