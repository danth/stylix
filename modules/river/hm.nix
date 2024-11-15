{ config, lib, ... }:

{
  options.stylix.targets.river.enable =
    config.lib.stylix.mkEnableTarget "River" true;

  config = lib.mkIf (config.stylix.enable && config.stylix.targets.river.enable) {
    wayland.windowManager.river.settings = {
      border-color-focused = "0x${config.lib.stylix.colors.base0D}";
      border-color-unfocused = "0x${config.lib.stylix.colors.base03}";
      border-color-urgent = "0x${config.lib.stylix.colors.base08}";
      xcursor-theme = config.stylix.cursor.name;
    };
  };
}
