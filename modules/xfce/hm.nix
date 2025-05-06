{ config, lib, ... }:

{
  # Disabled by default due to https://github.com/danth/stylix/issues/180
  options.stylix.targets.xfce.enable =
    config.lib.stylix.mkEnableTarget "Xfce" false;

  config = lib.mkIf (config.stylix.enable && config.stylix.targets.xfce.enable) {
    xfconf.settings = {
      xfwm4 = with config.stylix.fonts; {
        "general/title_font" = "${sansSerif.name} ${toString sizes.desktop}";
      };
      xsettings = with config.stylix.fonts; {
        "Gtk/FontName" = "${sansSerif.name} ${toString sizes.applications}";
        "Gtk/MonospaceFontName" = "${monospace.name} ${toString sizes.applications}";
      };
      xfce4-terminal = with config.lib.stylix.colors.withHashtag; {
        "color-palette" = lib.strings.concatStringsSep ";" [
          base00
          base08
          base0B
          base0A
          base0D
          base0E
          base0C
          base05
          base02
          base08
          base0B
          base0A
          base0D
          base0E
          base0C
          base07
        ];
        "color-foreground" = base05;
        "color-background" = base00;
        "color-bold-is-bright" = false;
      };
    };
  };
}
