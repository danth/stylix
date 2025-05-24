{ mkTarget, ... }:
mkTarget {
  name = "xfce";
  humanName = "Xfce";
  # Disabled by default due to https://github.com/nix-community/stylix/issues/180
  autoEnable = false;

  configElements =
    { fonts }:
    {
      xfconf.settings = with fonts; {
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
