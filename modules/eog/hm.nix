{ mkTarget, ... }:
mkTarget {
  name = "eog";
  humanName = "Eye of GNOME Image Viewer";

  configElements =
    { colors }:
    {
      dconf.settings."org/gnome/eog/view" = {
        # transparency = "background"; # Disables the grey and white check pattern.
        background-color = "#${colors.base00}";
      };
    };
}
