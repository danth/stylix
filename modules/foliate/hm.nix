{ config, lib, ... }:

with config.lib.stylix.colors;

{
  options.stylix.targets.foliate.enable =
    config.lib.stylix.mkEnableTarget "Foliate" true;

  config =
    lib.mkIf (config.stylix.enable && config.stylix.targets.foliate.enable)
      {

        # Generate the theme
        xdg.configFile."com.github.johnfactotum.Foliate/themes/stylix.json".text =
          builtins.toJSON
            {
              label = "Stylix";
              light = {
                fg = "#${base00}";
                bg = "#${base05}";
                link = "#${base0D}";
              };
              dark = {
                fg = "#${base05}";
                bg = "#${base00}";
                link = "#${base0D}";
              };
            };

        # Select the theme
        dconf.settings."com/github/johnfactotum/Foliate/viewer/view" = {
          theme = "stylix.json";
        };
      };
}
