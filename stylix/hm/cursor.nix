{ config, lib, pkgs, ... }:

let
  cfg = config.stylix.cursor;

in {
  imports = [ ../cursor.nix ];

  config = lib.mkIf (config.stylix.enable && pkgs.stdenv.hostPlatform.isLinux) {
    home.pointerCursor = {
      name = cfg.name;
      package = cfg.package;
      size = cfg.size;
      x11.enable = true;
      gtk.enable = true;
    };
  };
}
