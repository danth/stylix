{ config, ... }:

let
  cfg = config.stylix.cursor;
in {
  imports = [ ../cursor.nix ];
  config = {
    environment.variables.XCURSOR_SIZE = toString cfg.size;
  };
}
