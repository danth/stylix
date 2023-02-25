{ config, lib, ... }:

let
  colors = config.lib.stylix.colors.withHashtag;
in {
  options.stylix.targets.bspwm.enable =
    config.lib.stylix.mkEnableTarget "bspwm" true;

  config = lib.mkIf config.stylix.targets.bspwm.enable {
    xsession.windowManager.bspwm.settings = {
      active_border_color = colors.base08;
      normal_border_color = colors.base02;
      focused_border_color = colors.base0F;
      presel_feedback_color = colors.base08;
    };
  };
}
