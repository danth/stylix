{ mkTarget, ... }:
mkTarget {
  name = "font-packages";
  humanName = "Font packages";

  configElements =
    { fonts }:
    {
      home = {
        inherit (fonts) packages;
      };
    };
}
