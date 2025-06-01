{ mkTarget, ... }:
mkTarget {
  name = "gedit";
  humanName = "GEdit";

  configElements =
    { colors }:
    {
      xdg.dataFile = {
        "gedit/styles/stylix.xml".source = colors {
          template = ./template.xml.mustache;
          extension = ".xml";
        };
      };
    };
}
