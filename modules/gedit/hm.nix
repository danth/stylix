{ config, lib, ... }:

let
  style = config.lib.stylix.colors {
    template = ./template.mustache;
    extension = "xml";
  };

in
{
  options.stylix.targets.gedit.enable =
    config.lib.stylix.mkEnableTarget "GEdit" true;

  config = lib.mkIf (config.stylix.enable && config.stylix.targets.gedit.enable) {
    xdg.dataFile = {
      "gedit/styles/stylix.xml".source = style;
    };
  };
}
