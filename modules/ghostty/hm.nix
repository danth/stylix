# Documentation is available at:
# - https://ghostty.org/docs/config/reference
# - `man 5 ghostty`
{ mkTarget, ... }:
mkTarget {
  name = "ghostty";
  humanName = "Ghostty";

  configElements = [
    (
      { fonts }:
      {
        programs.ghostty.settings = {
          font-family = [
            fonts.monospace.name
            fonts.emoji.name
          ];
          font-size = fonts.sizes.terminal;
        };
      }
    )
    (
      { opacity }:
      {
        programs.ghostty.settings = {
          background-opacity = opacity.terminal;
        };
      }
    )
    (
      { colors }:
      {
        programs.ghostty = {
          settings.theme = "stylix";
          themes.stylix = {
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
      }
    )
  ];
}
