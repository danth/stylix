{ config, lib, ... }:

with config.lib.stylix.colors.withHashtag;
let
  dunstOpacity = lib.toHexString ((((builtins.ceil (config.stylix.opacity.popups * 100)) * 255) / 100));
  inherit (config.stylix) fonts;
  inherit (fonts) sizes;
  sansSerif = builtins.head sansSerif;
in {
  options.stylix.targets.dunst.enable =
    config.lib.stylix.mkEnableTarget "Dunst" true;

  config = lib.mkIf config.stylix.targets.dunst.enable {
    services.dunst.settings = {
      global = {
        separator_color = base02;
        font = "${sansSerif.name} ${toString sizes.popups}";
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
