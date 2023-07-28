{ config, ... }:

let
  cfg = config.stylix.fonts;
in {
  imports = [ ../fonts.nix ];
  config.fonts = {
    packages = [
      cfg.monospace.package
      cfg.serif.package
      cfg.sansSerif.package
      cfg.emoji.package
    ];

    fontconfig.defaultFonts = {
      monospace = [ cfg.monospace.name ];
      serif = [ cfg.serif.name ];
      sansSerif = [ cfg.sansSerif.name ];
      emoji = [ cfg.emoji.name ];
    };
  };
}
