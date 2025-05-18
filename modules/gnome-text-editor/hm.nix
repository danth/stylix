{ config, lib, ... }:
{
  config =
    lib.mkIf
      (config.stylix.enable && config.stylix.targets.gnome-text-editor.enable)
      {
        dconf.settings."org/gnome/TextEditor".style-scheme = "stylix";
      };
}
