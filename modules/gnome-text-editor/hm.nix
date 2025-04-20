{
  config,
  osConfig,
  lib,
  ...
}:

let
  inherit (lib) optional;
  inherit (builtins) any;
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
          optional
            (
              !(any (c: c.stylix.targets.gtksourceview.enable) [
                config
                osConfig
              ])
            )
            "stylix: gnome-text-editor: This module will probably not work because the `gtksourceview' target is not enabled.";
      };
}
