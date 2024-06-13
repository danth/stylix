{ config, lib, ... }:

{
  # Disabled by default due to https://github.com/danth/stylix/issues/180
  options.stylix.targets.xfce.enable =
    config.lib.stylix.mkEnableTarget "Xfce" false;

  config = lib.mkIf (config.stylix.enable && config.stylix.targets.xfce.enable) {
    xfconf.settings = with config.stylix.fonts; {
      xfwm4 = {
        "general/title_font" = "${sansSerif.name} ${toString sizes.desktop}";
      };
      xsettings = {
        "Gtk/FontName" = "${sansSerif.name} ${toString sizes.applications}";
        "Gtk/MonospaceFontName" = "${monospace.name} ${toString sizes.applications}";
      };
    };
  };
}
