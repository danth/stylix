{ config, lib, ... }:

{
  imports = [ ./gnome-text-editor.nix ];

  config =
    lib.mkIf
      (config.stylix.enable && config.stylix.targets.gnome-text-editor.enable)
      {
        dconf.settings."org/gnome/TextEditor".style-scheme = "stylix";
      };
}
