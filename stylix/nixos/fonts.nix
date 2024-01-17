{ config, lib, ... }:

let
  cfg = config.stylix.fonts;
in {
  imports = [ ../fonts.nix ];
  config.fonts = {
    packages = cfg.packages;

    fontconfig.defaultFonts = {
      monospace = lib.catAttrs "name" cfg.monospace;
      serif = lib.catAttrs "name" cfg.serif;
      sansSerif = lib.catAttrs "name" cfg.sansSerif;
      emoji = lib.catAttrs "name" cfg.emoji;
    };
  };
}
