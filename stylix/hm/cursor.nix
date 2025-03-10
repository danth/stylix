{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.stylix.cursor;

in
{
  config =
    lib.mkIf
      (
        config.stylix.enable
        && pkgs.stdenv.hostPlatform.isLinux
        && config.stylix.cursor != { }
      )
      {
        home.pointerCursor = {
          inherit (cfg) name package size;
          x11.enable = true;
          gtk.enable = true;
        };
      };
}
