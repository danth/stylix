{ config, lib, ... }:
{
  options.stylix.targets.bspwm.enable =
    config.lib.stylix.mkEnableTarget "bspwm" true;

  config = lib.mkIf (config.stylix.enable && config.stylix.targets.bspwm.enable) {
    xsession.windowManager.bspwm.settings =
      with config.lib.stylix.colors.withHashtag; {
        normal_border_color = base03;
        active_border_color = base0C;
        focused_border_color = base0D;
        presel_feedback_color = base00;
      };
  };
}
