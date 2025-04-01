{ config, lib, ... }:
let
  style = config.lib.stylix.colors {
    template = ../gedit/template.xml.mustache;
    extension = "xml";
  };
in
{
  options.stylix.targets.gnome-text-editor.enable =
    config.lib.stylix.mkEnableOverlay "GNOME Text Editor";

  overlay =
    _: prev:
    lib.optionalAttrs
      (config.stylix.enable && config.stylix.targets.gnome-text-editor.enable)
      {
        gnome-text-editor = prev.gnome-text-editor.overrideAttrs (oldAttrs: {
          postFixup = ''
            ${oldAttrs.postFixup or ""}
            cp ${style} $out/share/gnome-text-editor/styles/stylix.xml
          '';
        });
      };
}
