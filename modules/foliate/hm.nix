{ config, lib, ... }:

{
  options.stylix.targets.foliate.enable =
    config.lib.stylix.mkEnableTarget "Foliate" true;

  config =
    lib.mkIf (config.stylix.enable && config.stylix.targets.foliate.enable)
      {
        programs.foliate = {
          settings."viewer/view" = {
            theme = "stylix.json";
          };

          themes.stylix = with config.lib.stylix.colors.withHashtag; {
            label = "Stylix";
            light = {
              fg = base00;
              bg = base05;
              link = base0D;
            };
            dark = {
              fg = base05;
              bg = base00;
              link = base0D;
            };
          };
        };
      };
}
