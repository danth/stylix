{
  lib,
  config,
  ...
}:
let
  style = config.lib.stylix.colors {
    template = ../gedit/template.xml.mustache;
    extension = ".xml";
  };
in
{
  overlay =
    _: prev:
    lib.optionalAttrs
      (
        config.stylix.enable && config.stylix.targets.gnome-text-editor.enable or false
      )
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
