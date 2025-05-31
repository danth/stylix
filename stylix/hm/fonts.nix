{ config, lib, ... }:

let
  cfg = config.stylix.fonts;
in
{
  config = lib.mkIf config.stylix.enable {
    fonts.fontconfig = {
      enable = true;
      defaultFonts = {
        monospace = [ cfg.monospace.name ];
        serif = [ cfg.serif.name ];
        sansSerif = [ cfg.sansSerif.name ];
        emoji = [ cfg.emoji.name ];
      };
    };
    home.packages = cfg.packages;
  };
}
