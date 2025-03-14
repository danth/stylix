{ config, lib, ... }:

{
  options.stylix.targets.river.enable =
    config.lib.stylix.mkEnableTarget "River" true;

  config = lib.mkIf (config.stylix.enable && config.stylix.targets.river.enable) {
    wayland.windowManager.river.settings =
      let
        inherit (config.lib.stylix) colors;
        inherit (config.stylix) cursor;
      in
      {
        border-color-focused = "0x${colors.base0D}";
        border-color-unfocused = "0x${colors.base03}";
        border-color-urgent = "0x${colors.base08}";
        background-color = "0x${colors.base00}";
        xcursor-theme = lib.mkIf (
          config.stylix.cursor != null
        ) "${cursor.name} ${toString cursor.size}";
      };
  };
}
