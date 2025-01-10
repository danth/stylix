{ config, lib, ... }:

let
  colors = config.lib.stylix.colors.withHashtag;
in
{
  options.stylix.targets.bspwm.enable =
    config.lib.stylix.mkEnableTarget "bspwm" true;

  config = lib.mkIf (config.stylix.enable && config.stylix.targets.bspwm.enable) {
    xsession.windowManager.bspwm.settings = {
      normal_border_color = colors.base03;
      active_border_color = colors.base0C;
      focused_border_color = colors.base0D;
      presel_feedback_color = colors.base00;
    };
  };
}
