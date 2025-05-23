{ mkTarget, ... }:
mkTarget {
  name = "wob";
  humanName = "wob";

  configElements =
    { colors }:
    {
      services.wob.settings = {
        "" = with colors; {
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
