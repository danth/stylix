{ mkTarget, ... }:
mkTarget {
  name = "xresources";
  humanName = "Xresources";

  configElements = [
    (
      { fonts }:
      {
        xresources.properties = {
          "*.faceName" = fonts.monospace.name;
          "*.faceSize" = toString fonts.sizes.terminal;
          "*.renderFont" = true;
        };
      }
    )
    (
      { colors }:
      {
        xresources.properties = with colors.withHashtag; {
          "*foreground" = base05;
          "*background" = base00;
          "*cursorColor" = base05;
          "*color0" = base00;
          "*color1" = base08;
          "*color2" = base0B;
          "*color3" = base0A;
          "*color4" = base0D;
          "*color5" = base0E;
          "*color6" = base0C;
          "*color7" = base05;
          "*color8" = base03;
          "*color9" = base09;
          "*color10" = base01;
          "*color11" = base02;
          "*color12" = base04;
          "*color13" = base06;
          "*color14" = base0F;
          "*color15" = base07;
        };
      }
    )
  ];
}
