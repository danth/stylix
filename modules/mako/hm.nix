{ mkTarget, lib, ... }:
mkTarget {
  name = "mako";
  humanName = "Mako";

  # Referenced https://github.com/stacyharper/base16-mako
  configElements = [
    (
      { fonts }:
      {
        services.mako.settings.font = "${fonts.sansSerif.name} ${toString fonts.sizes.popups}";
      }
    )
    (
      { colors, opacity }:
      {
        services.mako =
          let
            makoOpacity = lib.toHexString (builtins.ceil (opacity.popups * 255));
          in
          with colors.withHashtag;
          {
            settings = {
              background-color = base00 + makoOpacity;
              border-color = base0D;
              text-color = base05;
              progress-color = "over ${base02}";

              "urgency=low" = {
                background-color = "${base00}${makoOpacity}";
                border-color = base03;
                text-color = base05;
              };
              "urgency=high" = {
                background-color = "${base00}${makoOpacity}";
                border-color = base08;
                text-color = base05;
              };
            };
          };
      }
    )
  ];
}
