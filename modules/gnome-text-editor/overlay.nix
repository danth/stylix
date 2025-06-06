{
  name = "gnome-text-editor";

  overlay =
    { colors }:
    let
      style = colors {
        template = ../gedit/template.xml.mustache;
        extension = ".xml";
      };
    in
    _: prev: {
      gnome-text-editor = prev.gnome-text-editor.overrideAttrs (oldAttrs: {
        postFixup = ''
          ${oldAttrs.postFixup or ""}
          mkdir -p $out/share/gtksourceview-5/styles
          cp ${style} $out/share/gtksourceview-5/styles/stylix.xml
        '';
      });
    };
}
