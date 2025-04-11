{ config, lib, ... }:

let
  inherit (lib) optionalAttrs;
  inherit (config.lib.stylix) colors mkEnableTarget;

  cfg = config.stylix.targets.gtksourceview;

  style = colors {
    template = ./template.xml.mustache;
    extension = "xml";
  };

  attrsOverride = version: oldAttrs: {
    postFixup = ''
      ${oldAttrs.postFixup or ""}
      styles_dir="$out/share/gtksourceview-${version}/styles"
      mkdir --parents "$styles_dir"
      cp ${style} "$styles_dir/stylix.xml"
    '';
  };
in
{
  options.stylix.targets.gtksourceview = {
    enable = mkEnableTarget "GTKSourceView" true;
    overlay.enable = mkEnableTarget "the overlay" (!config ? home);
  };

  overlay =
    _final: prev:
    optionalAttrs (config.stylix.enable && cfg.enable && cfg.overlay.enable) {
      gnome2.gtksourceview = prev.gnome2.gtksourceview.overrideAttrs (
        attrsOverride "2.0"
      );
      gtksourceview = prev.gtksourceview.overrideAttrs (attrsOverride "3.0");
      gtksourceview4 = prev.gtksourceview4.overrideAttrs (attrsOverride "4");
      gtksourceview5 = prev.gtksourceview5.overrideAttrs (attrsOverride "5");
    };
}
