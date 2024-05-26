{ pkgs, config, lib, ... }@args:

{
  # Disabled by default due to https://github.com/danth/stylix/issues/180
  options.stylix.targets.xfce.enable =
    config.lib.stylix.mkEnableTarget "Xfce" false;

  config = lib.mkIf config.stylix.targets.xfce.enable {
    xfconf.settings = let
      inherit (config.stylix) fonts;
      inherit (fonts) sizes;
      sansSerif = builtins.head fonts.sansSerif;
      monospace = builtins.head fonts.monospace;
    in {
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
