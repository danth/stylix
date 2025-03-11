{ config, lib, ... }:

let
  style = config.lib.stylix.colors {
    template = ./template.xml.mustache;
    extension = "xml";
  };

  attrsOverride = oldAttrs: {
    postFixup = ''
      ${oldAttrs.postFixup or ""}
      styles_dir="$out/share/gtksourceview-3.0/styles"
      mkdir --parents "$styles_dir"
      cp ${style} "$styles_dir/stylix.xml"
    '';
  };

in
{
  options.stylix.targets.gtksourceview.enable =
    config.lib.stylix.mkEnableTarget "GTKSourceView" true;

  config =
    lib.mkIf (config.stylix.enable && config.stylix.targets.gtksourceview.enable)
      {
        nixpkgs.overlays = [
          (_: prev: {
            gtksourceview = prev.gtksourceview.overrideAttrs attrsOverride;
            gtksourceview4 = prev.gtksourceview4.overrideAttrs attrsOverride;
            gtksourceview5 = prev.gtksourceview5.overrideAttrs attrsOverride;
          })
        ];
      };
}
