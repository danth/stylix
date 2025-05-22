{ mkTarget, ... }:
mkTarget {
  name = "bspwm";
  humanName = "bspwm";

  configElements =
    { colors }:
    {
      xsession.windowManager.bspwm.settings = with colors.withHashtag; {
        normal_border_color = base03;
        active_border_color = base0C;
        focused_border_color = base0D;
        presel_feedback_color = base00;
      };
    };
}
