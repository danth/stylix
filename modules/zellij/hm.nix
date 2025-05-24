{ mkTarget, ... }:
mkTarget {
  name = "zellij";
  humanName = "zellij";

  configElements =
    { colors }:
    {
      programs.zellij.themes.stylix = {
        themes = with colors.withHashtag; {
          default = {
            bg = base03;
            fg = base05;
            red = base01;
            green = base0B;
            blue = base0D;
            yellow = base0A;
            magenta = base0E;
            orange = base09;
            cyan = base0C;
            black = base00;
            white = base07;
          };
        };
      };
    };
}
