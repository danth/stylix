{ pkgs, config, lib, ... }@args:

{
  options.stylix.targets.xfce.enable =
    config.lib.stylix.mkEnableTarget "Xfce" pkgs.stdenv.hostPlatform.isLinux;

  config = lib.mkIf config.stylix.targets.xfce.enable {
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
