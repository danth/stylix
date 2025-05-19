{ config, lib, ... }:

{
  options.stylix.targets.river.enable =
    config.lib.stylix.mkEnableTarget "River" true;

  config = lib.mkIf (config.stylix.enable && config.stylix.targets.river.enable) {
    wayland.windowManager.river.settings =
      let
        inherit (config.lib.stylix) colors mkHexColor;
        inherit (config.stylix) cursor;
      in
      {
        border-color-focused = mkHexColor colors.base0D;
        border-color-unfocused = mkHexColor colors.base03;
        border-color-urgent = mkHexColor colors.base08;
        background-color = mkHexColor colors.base00;
        xcursor-theme = lib.mkIf (
          config.stylix.cursor != null
        ) "${cursor.name} ${toString cursor.size}";
      };
  };
}
