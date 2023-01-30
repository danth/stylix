{ config, ... }:

let
  cfg = config.stylix.fonts;
in {
  imports = [ ../fonts.nix ];
  config = {
    fonts.fontconfig.enable = true;
    home.packages = [
      cfg.monospace.package
      cfg.serif.package
      cfg.sansSerif.package
      cfg.emoji.package
    ];
  };
}
