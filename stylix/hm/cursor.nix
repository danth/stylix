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
        && config.stylix.cursor != null
        && pkgs.stdenv.hostPlatform.isLinux
      )
      {
        home.pointerCursor = {
          inherit (cfg) name package;
          size = builtins.floor (cfg.size + 0.5);
          x11.enable = true;
          gtk.enable = true;
        };
      };
}
