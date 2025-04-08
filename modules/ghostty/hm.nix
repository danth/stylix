# Documentation is available at:
# - https://ghostty.org/docs/config/reference
# - `man 5 ghostty`
{ config, lib, ... }:
{
  options.stylix.targets.ghostty.enable =
    config.lib.stylix.mkEnableTarget "Ghostty" true;

  config =
    lib.mkIf (config.stylix.enable && config.stylix.targets.ghostty.enable)
      {
        programs.ghostty = {
          settings =
            let
              inherit (config.stylix) fonts opacity;
            in
            {
              theme = "stylix";
              font-family = [
                fonts.monospace.name
                fonts.emoji.name
              ];
              font-size = fonts.sizes.terminal;
              background-opacity = opacity.terminal;
            };
          themes.stylix =
            let
              inherit (config.lib.stylix) colors;
            in
            {
              background = colors.base00;
              foreground = colors.base05;
              cursor-color = colors.base05;
              selection-background = colors.base02;
              selection-foreground = colors.base05;

              palette = with colors.withHashtag; [
                "0=${base00}"
                "1=${base08}"
                "2=${base0B}"
                "3=${base0A}"
                "4=${base0D}"
                "5=${base0E}"
                "6=${base0C}"
                "7=${base05}"
                "8=${base03}"
                "9=${base08}"
                "10=${base0B}"
                "11=${base0A}"
                "12=${base0D}"
                "13=${base0E}"
                "14=${base0C}"
                "15=${base07}"
              ];
            };
        };
      };
}
