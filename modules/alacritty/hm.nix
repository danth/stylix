# Documentation is available at https://alacritty.org/config-alacritty.html.
{ config, lib, ... }:

let
  colors = config.lib.stylix.colors;
  mnemonicsWithHashtag = builtins.mapAttrs (_: color: "#${color}") colors;
in
{
  options.stylix.targets.alacritty.enable = config.lib.stylix.mkEnableTarget "Alacritty" true;

  config = lib.mkIf config.stylix.targets.alacritty.enable {
    programs.alacritty.settings = {
      font = with config.stylix.fonts; {
        normal = {
          family = monospace.name;
          style = "Regular";
        };
        size = sizes.terminal;
      };
      window.opacity = with config.stylix.opacity; terminal;
      colors = with colors.withHashtag; {
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
          inherit (mnemonicsWithHashtag)
            red
            green
            yellow
            blue
            magenta
            cyan
            ;
        };
        bright = with mnemonicsWithHashtag; {
          black = base03;
          white = base07;
          red = bright-red;
          green = bright-green;
          yellow = yellow;
          blue = bright-blue;
          magenta = bright-magenta;
          cyan = bright-cyan;
        };
      };
    };
  };
}
