{ config, lib, ... }:
let
  style = config.lib.stylix.colors {
    template = ../gedit/template.xml.mustache;
    extension = ".xml";
  };
in
{
  options.stylix.targets.gnome-text-editor.enable =
    config.lib.stylix.mkEnableTarget "GNOME Text Editor" true;

  overlay =
    _: prev:
    lib.optionalAttrs
      (config.stylix.enable && config.stylix.targets.gnome-text-editor.enable)
      {
        gnome-text-editor = prev.gnome-text-editor.overrideAttrs (oldAttrs: {
          postFixup = ''
            ${oldAttrs.postFixup or ""}
            mkdir -p $out/share/gtksourceview-5/styles
            cp ${style} $out/share/gtksourceview-5/styles/stylix.xml
          '';
        });
      };
}
