{ config, ... }:

let
  cfg = config.stylix.fonts;
in {
  imports = [ ../fonts.nix ];
  config.fonts = {
    fontDir.enable = true;
    fonts = cfg.packages;
  };
}
