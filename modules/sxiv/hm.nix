{ config, lib, ... }:

let
  inherit (config.lib.stylix) colors;
  inherit (config.stylix) fonts;
  sansSerif = builtins.head fonts.sansSerif;
in {
  options.stylix.targets.sxiv.enable =
    config.lib.stylix.mkEnableTarget "Sxiv" true;

  config = lib.mkIf config.stylix.targets.sxiv.enable {
    xresources = {
      properties = {
        "Sxiv.foreground" = "#${colors.base01}";
        "Sxiv.background" = "#${colors.base04}";
        "Sxiv.font" = "${sansSerif.name}-${toString fonts.sizes.applications}";
      };
    };
  };
}
