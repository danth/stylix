{ config, lib, ... }:

let
  cfg = config.stylix.fonts;
in {
  imports = [ ../fonts.nix ];
  config.fonts = lib.mkIf config.stylix.enable {
    fontDir.enable = true;
    fonts = cfg.packages;
  };
}
