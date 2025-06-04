{ mkTarget, ... }:
mkTarget {
  name = "font-packages";
  humanName = "Font packages";

  configElements =
    { fonts }:
    {
      fonts = {
        inherit (fonts) packages;
      };
    };
}
