{ config, lib, ... }:

let
  style = config.lib.stylix.colors {
    template = ./template.xml.mustache;
    extension = "xml";
  };

in
{
  options.stylix.targets.gtksourceview.enable =
    config.lib.stylix.mkEnableTarget "GTKSourceView" true;

  config =
    lib.mkIf (config.stylix.enable && config.stylix.targets.gtksourceview.enable)
      {
        xdg.dataFile = {
          "gtksourceview-2.0/styles/stylix.xml".source = style;
          "gtksourceview-3.0/styles/stylix.xml".source = style;
          "gtksourceview-4/styles/stylix.xml".source = style;
          "gtksourceview-5/styles/stylix.xml".source = style;
        };
      };
}
