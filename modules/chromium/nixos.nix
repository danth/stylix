{ mkTarget, ... }:
mkTarget {
  name = "chromium";
  humanName = "Chromium, Google Chrome and Brave";

  configElements =
    { colors }:
    {
      programs.chromium = {
        # This enables policies without installing the browser. Policies take up a
        # negligible amount of space, so it's reasonable to have this always on.
        enable = true;

        extraOpts.BrowserThemeColor = colors.withHashtag.base00;
      };
    };
}
