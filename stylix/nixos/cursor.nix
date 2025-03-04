{ config, lib, ... }:

{
  config = lib.mkIf (config.stylix.enable && config.stylix.cursor != { }) {
    environment.variables.XCURSOR_SIZE = toString config.stylix.cursor.size;
  };
}
