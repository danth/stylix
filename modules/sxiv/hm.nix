{ mkTarget, ... }:
mkTarget {
  name = "sxiv";
  humanName = "Sxiv";

  configElements = [
    (
      { fonts }:
      {
        xresources.properties."Sxiv.font" =
          "${fonts.sansSerif.name}-${toString fonts.sizes.applications}";
      }
    )
    (
      { colors }:
      {
        xresources.properties = {
          "Sxiv.foreground" = "#${colors.base01}";
          "Sxiv.background" = "#${colors.base04}";
        };
      }
    )
  ];
}
