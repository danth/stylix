{ config, lib, ... }:

{
  options.stylix.targets.kitty.enable =
    config.lib.stylix.mkEnableTarget "Kitty" true;

  config = lib.mkIf config.stylix.targets.kitty.enable {
    home-manager.sharedModules = [{
      programs.kitty = {
        font = config.stylix.fonts.monospace;

        settings = with config.lib.stylix.colors.withHashtag; {
          # Based on https://github.com/kdrag0n/base16-kitty/
          active_border_color = base03;
          active_tab_background = base00;
          active_tab_foreground = base05;
          background = base00;
          cursor = base05;
          foreground = base05;
          inactive_border_color = base01;
          inactive_tab_background = base01;
          inactive_tab_foreground = base04;
          selection_background = base05;
          selection_foreground = base00;
          tab_bar_background = base01;
          url_color = base04;

          color0 = base00;
          color1 = base08;
          color2 = base0B;
          color3 = base0A;
          color4 = base0D;
          color5 = base0E;
          color6 = base0C;
          color7 = base05;
          color8 = base03;
          color9 = base09;
          color10 = base01;
          color11 = base02;
          color12 = base04;
          color13 = base06;
          color14 = base0F;
          color15 = base07;
        };
      };
    }];
  };
}
