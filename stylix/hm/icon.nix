{ config, lib, ... }:

let
  cfg = config.stylix.iconTheme;
  inherit (config.stylix) polarity;
in
{
  config = lib.mkIf (config.stylix.enable && cfg.enable) {
    gtk = {
      iconTheme = {
        inherit (cfg) package;
        name = builtins.head (
          lib.filter (x: null != x) [
            (
              {
                inherit (cfg) dark light;
              }
              ."${polarity}" or null
            )
            cfg.dark
            cfg.light
          ]
        );
      };
    };
  };
}
