{
  config,
  lib,
  ...
}: let
  theme = config.lib.stylix.colors {
    template = ./theme.conf.mustache;
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
    config.lib.stylix.mkEnableTarget
    "fcitx5"
    true;

  config =
    lib.mkIf (
      config.stylix.enable
      && config.stylix.targets.fcitx5.enable
      #&& config.i18n.inputMethod.enable
      #&& (config.i18n.inputMethod.type == "fcitx5")
    ) {
      xdg.dataFile."fcitx5/themes/stylix/theme.conf".source = theme;
      xdg.dataFile."fcitx5/themes/stylix/panel.svg".source = panel;
      xdg.dataFile."fcitx5/themes/stylix/highlight.svg".source = highlight;
    };
}
