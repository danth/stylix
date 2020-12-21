{ config, ... }:

{
  stylix.homeModule = {
    programs.kitty = {
      font = config.stylix.fonts.monospace;

      settings = with config.lib.stylix.colors; {
        # Based on https://github.com/kdrag0n/base16-kitty/
        active_border_color = base03-hash;
        active_tab_background = base00-hash;
        active_tab_foreground = base05-hash;
        background = base00-hash;
        cursor = base05-hash;
        foreground = base05-hash;
        inactive_border_color = base01-hash;
        inactive_tab_background = base01-hash;
        inactive_tab_foreground = base04-hash;
        selection_background = base05-hash;
        selection_foreground = base00-hash;
        tab_bar_background = base01-hash;
        url_color = base04-hash;

        color0 = base00-hash;
        color1 = base08-hash;
        color2 = base0B-hash;
        color3 = base0A-hash;
        color4 = base0D-hash;
        color5 = base0E-hash;
        color6 = base0C-hash;
        color7 = base05-hash;
        color8 = base03-hash;
        color9 = base09-hash;
        color10 = base01-hash;
        color11 = base02-hash;
        color12 = base04-hash;
        color13 = base06-hash;
        color14 = base0F-hash;
        color15 = base07-hash;
      };
    };
  };
}
