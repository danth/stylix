{ config, lib, ... }:

{
  options.stylix.targets.xresources.enable =
    config.lib.stylix.mkEnableTarget "Xresources" true;

  config =
    lib.mkIf (config.stylix.enable && config.stylix.targets.xresources.enable)
      {
        xresources.properties =
          with config.lib.stylix.colors.withHashtag;
          with config.stylix.fonts;
          {
            "*.faceName" = monospace.name;
            "*.faceSize" = toString sizes.terminal;
            "*.renderFont" = true;
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
      };
}
