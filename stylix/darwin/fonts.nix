{ config, ... }:

let
  cfg = config.stylix.fonts;
  fallbackFontPackages = builtins.mapAttrs (_: builtins.map ({ package, ... }: package)) cfg.fallbackFonts;
in {
  imports = [ ../fonts.nix ];
  config.fonts = {
    fontDir.enable = true;

    fonts = [
      cfg.monospace.package
      cfg.serif.package
      cfg.sansSerif.package
      cfg.emoji.package
    ] ++ fallbackFontPackages.monospace
      ++ fallbackFontPackages.serif
      ++ fallbackFontPackages.sansSerif
      ++ fallbackFontPackages.emoji;
  };
}
