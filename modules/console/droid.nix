{ config, lib, ... }:

{
  options.stylix.targets.console.enable =
    config.lib.stylix.mkEnableTarget "the Nix-on-Droid console" true;

  config =
    lib.mkIf (config.stylix.enable && config.stylix.targets.console.enable)
      {
        terminal.colors = with config.lib.stylix.colors.withHashtag; rec {
          background = base00;
          foreground = base05;
          cursor = base05;

          # normal
          color0 = base00;
          color1 = base08;
          color2 = base0B;
          color3 = base0A;
          color4 = base0D;
          color5 = base0E;
          color6 = base0C;
          color7 = base05;

          # bright
          color8 = base02;
          color9 = color1;
          color10 = color2;
          color11 = color3;
          color12 = color4;
          color13 = color5;
          color14 = color6;
          color15 = base07;
        };
      };
}
