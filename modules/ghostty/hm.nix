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
              inherit (config.lib.stylix.colors) withHashtag;
            in
            {
              palette = [
                "0=${withHashtag.base03}"
                "1=${withHashtag.base08}"
                "2=${withHashtag.base0B}"
                "3=${withHashtag.base0A}"
                "4=${withHashtag.base0D}"
                "5=${withHashtag.base0F}"
                "6=${withHashtag.base0C}"
                "7=${withHashtag.base05}"
                "8=${withHashtag.base04}"
                "9=${withHashtag.base08}"
                "10=${withHashtag.base0B}"
                "11=${withHashtag.base0A}"
                "12=${withHashtag.base0D}"
                "13=${withHashtag.base0F}"
                "14=${withHashtag.base0C}"
                "15=${withHashtag.base05}"
              ];
              background = colors.base00;
              foreground = colors.base05;
              cursor-color = colors.base06;
              selection-background = colors.base02;
              selection-foreground = colors.base05;
            };
        };
      };
}
