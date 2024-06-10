{ config, lib, ... }:
let
  themeFile = config.lib.stylix.colors {
    template = ./template.mustache;
    extension = ".css";
  };
in
{
  options.stylix.targets.vesktop.enable = config.lib.stylix.mkEnableTarget "Vesktop" true;

  config = lib.mkIf (config.stylix.enable && config.stylix.targets.vesktop.enable) {
    home.file."${config.xdg.configHome}/vesktop/themes/stylix.theme.css" = {
      source = themeFile;
    };
  };
}
