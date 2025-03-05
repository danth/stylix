{ config, lib, ... }:

let
  style = config.lib.stylix.colors {
    template = ./template.xml.mustache;
    extension = "xml";
  };

in
{
  options.stylix.targets.gtksourceview.enable =
    config.lib.stylix.mkEnableTarget "GTKSourceView, which is used by GNOME Text Editor among others" true;

  config =
    lib.mkIf (config.stylix.enable && config.stylix.targets.gtksourceview.enable)
      {
        xdg.dataFile = {
          "gtksourceview/styles/stylix.xml".source = style;
        };
      };
}
