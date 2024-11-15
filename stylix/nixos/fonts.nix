{ config, lib, ... }:

let
  cfg = config.stylix.fonts;
in
{
  imports = [ ../fonts.nix ];
  config.fonts = lib.mkIf config.stylix.enable {
    inherit (cfg) packages;

    fontconfig.defaultFonts = {
      monospace = [ cfg.monospace.name ];
      serif = [ cfg.serif.name ];
      sansSerif = [ cfg.sansSerif.name ];
      emoji = [ cfg.emoji.name ];
    };
  };
}
