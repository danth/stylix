{ config, lib, ... }:

let
  cfg = config.stylix.fonts;
in
{
  config.fonts = lib.mkIf config.stylix.enable {
    fontconfig.defaultFonts = {
      monospace = [ cfg.monospace.name ];
      serif = [ cfg.serif.name ];
      sansSerif = [ cfg.sansSerif.name ];
      emoji = [ cfg.emoji.name ];
    };
  };
}
