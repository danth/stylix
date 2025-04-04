{ config, lib, ... }:
{
  options.stylix.targets.dunst.enable =
    config.lib.stylix.mkEnableTarget "Dunst" true;

  config = lib.mkIf (config.stylix.enable && config.stylix.targets.dunst.enable) {
    services.dunst.settings =
      with config.lib.stylix.colors.withHashtag;
      let
        inherit (config.stylix) fonts;
        dunstOpacity = lib.toHexString (
          ((builtins.floor (config.stylix.opacity.popups * 100 + 0.5)) * 255) / 100
        );
      in
      {
        global = {
          separator_color = base02;
          font = "${fonts.sansSerif.name} ${toString fonts.sizes.popups}";
        };

        urgency_low = {
          background = base01 + dunstOpacity;
          foreground = base05;
          frame_color = base0B;
        };

        urgency_normal = {
          background = base01 + dunstOpacity;
          foreground = base05;
          frame_color = base0E;
        };

        urgency_critical = {
          background = base01 + dunstOpacity;
          foreground = base05;
          frame_color = base08;
        };
      };
  };
}
