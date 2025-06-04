{ mkTarget, ... }:
mkTarget {
  name = "fontconfig";
  humanName = "Fontconfig";

  configElements = {
    fonts.fontconfig.enable = true;
  };
}
