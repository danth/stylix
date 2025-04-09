{ config, lib, ... }:

{
  options.stylix.targets.wob.enable = config.lib.stylix.mkEnableTarget "wob" true;

  config = lib.mkIf (config.stylix.enable && config.stylix.targets.wob.enable) {
    services.wob.settings = {
      "" = with config.lib.stylix.colors; {
        border_color = base05;
        background_color = base00;
        bar_color = base0A;
        overflow_bar_color = base08;
        overflow_background_color = base00;
        overflow_border_color = base05;
      };
    };
  };
}
