{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.stylix.cursor;

in {
  imports = [ ../cursor.nix ];

  config = mkIf ((builtins.match ".*-linux" "${pkgs.system}") != null) {
    home.pointerCursor = {
      name = "${cfg.name}";
      package = cfg.package;
      size = cfg.size;
      x11 = {
        enable = true;
        defaultCursor = "${cfg.name}";
      };
      gtk.enable = true;
    };
  };
}
