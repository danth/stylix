{ config, lib, ... }:

let
  colors = config.lib.stylix.colors;
  fonts = config.stylix.fonts;
in {
  options.stylix.targets.sxiv.enable =
    config.lib.stylix.mkEnableTarget "Sxiv" true;

  config = lib.mkIf config.stylix.targets.sxiv.enable {
    xresources = {
      properties = {
        "Sxiv.foreground" = "#${colors.base01}";
        "Sxiv.background" = "#${colors.base04}";
        "Sxiv.font" = "${fonts.sansSerif.name}-${toString fonts.sizes.applications}";
      };
    };
  };
}
