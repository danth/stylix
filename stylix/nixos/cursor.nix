{ config, lib, ... }:

{
  config = lib.mkIf (config.stylix.enable && config.stylix.cursor != null) {
    environment.variables.XCURSOR_SIZE = toString config.stylix.cursor.size;
  };
}
