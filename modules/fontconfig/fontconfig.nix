{ mkTarget }:
mkTarget {
  name = "fontconfig";
  humanName = "Fontconfig";

  configElements =
    { fonts }:
    {
      fonts.fontconfig.defaultFonts = {
        monospace = [ fonts.monospace.name ];
        serif = [ fonts.serif.name ];
        sansSerif = [ fonts.sansSerif.name ];
        emoji = [ fonts.emoji.name ];
      };
    };
}
