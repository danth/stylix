{ config, lib, ... }:

with config.stylix;
with config.lib.stylix.colors;
{
  options.stylix.targets.river.enable = config.lib.stylix.mkEnableTarget "River" true;

  config = lib.mkMerge [
    (lib.mkIf (config.stylix.enable && config.stylix.targets.river.enable) {
      wayland.windowManager.river.settings = {
        border-color-focused = "0x${base0D}";
        border-color-unfocused = "0x${base03}";
        border-color-urgent = "0x${base08}";
        xcursor-theme = cursor.name;
      };
    })
  ];
}
