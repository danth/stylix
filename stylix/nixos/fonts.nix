{ config, ... }:

let
  cfg = config.stylix.fonts;
  fallbackFontPackages = builtins.mapAttrs (_: builtins.map ({ package, ... }: package)) cfg.fallbackFonts;
  fallbackFontNames = builtins.mapAttrs (_: builtins.map ({ name, ... }: name)) cfg.fallbackFonts;
in {
  imports = [ ../fonts.nix ];
  config.fonts = {
    packages = [
      cfg.monospace.package
      cfg.serif.package
      cfg.sansSerif.package
      cfg.emoji.package
    ] ++ fallbackFontPackages.monospace
      ++ fallbackFontPackages.serif
      ++ fallbackFontPackages.sansSerif
      ++ fallbackFontPackages.emoji;

    fontconfig.defaultFonts = {
      monospace = [ cfg.monospace.name ] ++ fallbackFontNames.monospace;
      serif = [ cfg.serif.name ] ++ fallbackFontNames.serif;
      sansSerif = [ cfg.sansSerif.name ] ++ fallbackFontNames.sansSerif;
      emoji = [ cfg.emoji.name ] ++ fallbackFontNames.emoji;
    };
  };
}
