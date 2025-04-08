{ config, lib, ... }:

let
  cfg = config.stylix.fonts;
in
{
  config = lib.mkIf config.stylix.enable {
    fonts.fontconfig.enable = true;
    home.packages = cfg.packages;
  };
}
