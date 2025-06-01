{ mkTarget, ... }:
mkTarget {
  name = "foliate";
  humanName = "Foliate";

  configElements =
    { colors }:
    {
      programs.foliate = {
        settings."viewer/view" = {
          theme = "stylix.json";
        };

        themes.stylix = with colors.withHashtag; {
          label = "Stylix";
          light = {
            fg = base00;
            bg = base05;
            link = base0D;
          };
          dark = {
            fg = base05;
            bg = base00;
            link = base0D;
          };
        };
      };
    };
}
