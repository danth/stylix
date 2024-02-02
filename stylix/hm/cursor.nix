{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.stylix.cursor;

in {
  imports = [ ../cursor.nix ];

  config = mkIf (config.stylix.enable && pkgs.stdenv.hostPlatform.isLinux) {
    home.pointerCursor = {
      name = cfg.name;
      package = cfg.package;
      size = cfg.size;
      x11.enable = true;
      gtk.enable = true;
    };
  };
}
