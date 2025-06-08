{ mkTarget, ... }:
mkTarget {
  name = "gnome-text-editor";
  humanName = "GNOME Text Editor";

  configElements = {
    dconf.settings."org/gnome/TextEditor".style-scheme = "stylix";
  };
}
