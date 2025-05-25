{ mkTarget, lib, ... }:
mkTarget {
  name = "spotify-player";
  humanName = "spotify-player";

  configElements =
    { colors }:
    {
      programs.spotify-player = {
        settings.theme = "stylix";
        themes = lib.singleton {
          name = "stylix";
          palette = with colors.withHashtag; {
            background = base00;
            foreground = base05;
            black = base00;
            red = base08;
            green = base0B;
            yellow = base0A;
            blue = base0D;
            magenta = base0E;
            cyan = base0C;
            white = base05;
            bright_black = base03;
            bright_red = base08;
            bright_green = base0B;
            bright_yellow = base0A;
            bright_blue = base0D;
            bright_magenta = base0E;
            bright_cyan = base0C;
            bright_white = base07;
          };
        };
      };
    };
}
