{ config, lib, ... }:

{
  config = lib.mkIf config.stylix.enable {
    environment.variables.XCURSOR_SIZE = toString config.stylix.cursor.size;
  };
}
