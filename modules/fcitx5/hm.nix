{
  config,
  lib,
  ...
}: let
  # Adapted from https://github.com/sanweiya/fcitx5-mellow-themes under the BSD 2 license (compatible with this project's license (MIT))
  theme = config.lib.stylix.colors {
    template = ./theme.conf.mustache;
    extension = "conf";
  };
  theme-kwin = config.lib.stylix.colors {
    template = ./theme-kwin.conf.mustache;
    extension = "conf";
  };
  highlight = config.lib.stylix.colors {
    template = ./highlight.svg.mustache;
    extension = "svg";
  };
  panel = config.lib.stylix.colors {
    template = ./panel.svg.mustache;
    extension = "svg";
  };
in {
  options.stylix.targets.fcitx5.enable =
    config.lib.stylix.mkEnableTarget "fcitx5" true;

  config =
    lib.mkIf
    (config.stylix.enable && config.stylix.targets.fcitx5.enable)
    {
      xdg.dataFile = {
        "fcitx5/themes/stylix/theme.conf".source = theme;
        "fcitx5/themes/stylix/panel.svg".source = panel;
        "fcitx5/themes/stylix/highlight.svg".source = highlight;
        "fcitx5/themes/stylix-kwin/theme.conf".source = theme-kwin;
        "fcitx5/themes/stylix-kwin/panel.svg".source = panel;
        "fcitx5/themes/stylix-kwin/highlight.svg".source = highlight;
      };
    };
}
