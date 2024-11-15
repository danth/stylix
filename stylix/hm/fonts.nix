{ config, lib, ... }:

let
  cfg = config.stylix.fonts;
in
{
  imports = [ ../fonts.nix ];
  config = lib.mkIf config.stylix.enable {
    fonts.fontconfig.enable = true;
    home.packages = cfg.packages;
  };
}
