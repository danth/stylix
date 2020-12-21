{ config, ... }:

{
  stylix.homeModule = {
    programs.kitty = {
      font = config.stylix.fonts.monospace;

      settings = with config.lib.stylix.colors; {
        # Based on https://github.com/kdrag0n/base16-kitty/
        active_border_color = base03-hex;
        active_tab_background = base00-hex;
        active_tab_foreground = base05-hex;
        background = base00-hex;
        cursor = base05-hex;
        foreground = base05-hex;
        inactive_border_color = base01-hex;
        inactive_tab_background = base01-hex;
        inactive_tab_foreground = base04-hex;
        selection_background = base05-hex;
        selection_foreground = base00-hex;
        tab_bar_background = base01-hex;
        url_color = base04-hex;

        color0 = base00-hex;
        color1 = base08-hex;
        color2 = base0B-hex;
        color3 = base0A-hex;
        color4 = base0D-hex;
        color5 = base0E-hex;
        color6 = base0C-hex;
        color7 = base05-hex;
        color8 = base03-hex;
        color9 = base09-hex;
        color10 = base01-hex;
        color11 = base02-hex;
        color12 = base04-hex;
        color13 = base06-hex;
        color14 = base0F-hex;
        color15 = base07-hex;
      };
    };
  };
}
