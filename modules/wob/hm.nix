{ config, lib, ... }:

{
  options.stylix.targets.wob.enable = config.lib.stylix.mkEnableTarget "wob" true;

  config = lib.mkIf (config.stylix.enable && config.stylix.targets.wob.enable) {
    services.wob.settings = {
      "" = {
        border_color = config.lib.stylix.colors.base05;
        background_color = config.lib.stylix.colors.base00;
        bar_color = config.lib.stylix.colors.base0A;
        overflow_bar_color = config.lib.stylix.colors.base08;
        overflow_background_color = config.lib.stylix.colors.base00;
        overflow_border_color = config.lib.stylix.colors.base05;
      };
    };
  };
}
