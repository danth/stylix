# Documentation is available at:
# - https://alacritty.org/config-alacritty.html
# - `man 5 alacritty`
{ config, mkTarget, ... }:
mkTarget {
  name = "alacritty";
  humanName = "Alacritty";
  configElements = [
    (
      { colors }:
      with colors.withHashtag;
      {
        programs.alacritty.settings = {
          window.opacity = config.stylix.opacity.terminal;
          colors = with colors; {
            primary = {
              foreground = base05;
              background = base00;
              bright_foreground = base07;
            };
            selection = {
              text = base05;
              background = base02;
            };
            cursor = {
              text = base00;
              cursor = base05;
            };
            normal = {
              black = base00;
              white = base05;
              inherit
                red
                green
                yellow
                blue
                magenta
                cyan
                ;
            };
            bright = {
              black = base03;
              white = base07;
              red = bright-red;
              green = bright-green;
              yellow = bright-yellow;
              blue = bright-blue;
              magenta = bright-magenta;
              cyan = bright-cyan;
            };
          };
        };
      }
    )
    (
      { fonts }:
      {
        programs.alacritty.settings.font = {
          normal = {
            family = fonts.monospace.name;
            style = "Regular";
          };
          size = fonts.sizes.terminal;
        };
      }
    )
    (
      { opacity }:
      {
        programs.alacritty.settings.window.opacity = config.stylix.opacity.terminal;
      }
    )
  ];
}
