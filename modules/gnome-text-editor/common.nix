{ config, lib, ... }:

let
  style = config.lib.stylix.colors {
    template = ../gedit/template.mustache;
    extension = "xml";
  };
in
{
  options.stylix.targets.gnome-text-editor.enable =
    config.lib.stylix.mkEnableTarget "GNOME Text Editor" true;

  config =
    lib.mkIf
      (config.stylix.enable && config.stylix.targets.gnome-text-editor.enable)
      {
        nixpkgs.overlays = [
          (_: prev: {
            gnome-text-editor = prev.gnome-text-editor.overrideAttrs (oldAttrs: {
              postFixup = ''
                ${oldAttrs.postFixup or ""}
                cp ${style} $out/share/gnome-text-editor/styles/stylix.xml
              '';
            });
          })
        ];
      };
}
