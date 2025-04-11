{
  config,
  lib,
  ...
}:

let
  inherit (lib) optional;
in
{
  options.stylix.targets.gnome-text-editor.enable =
    config.lib.stylix.mkEnableTarget "GNOME Text Editor" true;

  config =
    lib.mkIf
      (config.stylix.enable && config.stylix.targets.gnome-text-editor.enable)
      {
        dconf.settings."org/gnome/TextEditor".style-scheme = "stylix";

        warnings =
          optional (!config.stylix.targets.gtksourceview.enable)
            "stylix: gnome-text-editor: This module will probably not work because the `gtksourceview' target is not enabled.";
      };
}
