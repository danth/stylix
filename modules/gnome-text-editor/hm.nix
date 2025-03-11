{
  config,
  options,
  lib,
  ...
}:

let
  inherit (lib) optional;
in
{
  config =
    lib.mkIf
      (config.stylix.enable && config.stylix.targets.gnome-text-editor.enable)
      {
        dconf.settings."org/gnome/TextEditor".style-scheme = "stylix";

        warnings =
          optional (!options.stylix.targets.gtksourceview.enable)
            "stylix: gnome-text-editor: This module will probably not work because the `gtksourceview' target is not enabled.";
      };
}
