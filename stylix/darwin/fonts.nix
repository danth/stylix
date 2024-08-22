{ config, lib, ... }:

let
  cfg = config.stylix.fonts;
in
{
  imports = [ ../fonts.nix ];
  config.fonts = lib.mkIf config.stylix.enable {
    inherit (cfg) packages;
  };
}
