{ mkTarget, ... }:
mkTarget {
  name = "bat";
  humanName = "Bat";

  configElements =
    { colors }:
    {
      programs.bat = {
        # This theme is reused for yazi. Changes to the template
        # will need to be applied to modules/yazi/hm.nix
        themes."base16-stylix".src = colors {
          template = ./base16-stylix.tmTheme.mustache;
          extension = ".tmTheme";
        };

        config.theme = "base16-stylix";
      };
    };
}
