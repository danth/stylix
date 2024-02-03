{ config, ... }:

let
  cfg = config.stylix.fonts;
in {
  imports = [ ../fonts.nix ];
  config = {
    fonts.fontconfig.enable = true;
    home.packages = cfg.packages;
  };
}
