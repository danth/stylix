{
  config,
  lib,
  options,
  ...
}:
{
  options.stylix.targets.mako.enable =
    config.lib.stylix.mkEnableTarget "Mako" true;

  # Referenced https://github.com/stacyharper/base16-mako
  config = lib.optionalAttrs (options.services ? mako) (
    lib.mkIf (config.stylix.enable && config.stylix.targets.mako.enable) {
      services.mako =
        let
          makoOpacity = lib.toHexString (
            ((builtins.ceil (config.stylix.opacity.popups * 100)) * 255) / 100
          );
          inherit (config.stylix) fonts;
        in
        with config.lib.stylix.colors.withHashtag;
        {
          settings = {
            background-color = base00 + makoOpacity;
            border-color = base0D;
            text-color = base05;
            progress-color = "over ${base02}";
            font = "${fonts.sansSerif.name} ${toString fonts.sizes.popups}";

            "urgency=low" = {
              background-color = "${base00}${makoOpacity}";
              border-color = base0D;
              text-color = base0A;
            };
            "urgency=high" = {
              background-color = "${base00}${makoOpacity}";
              border-color = base0D;
              text-color = base08;
            };
          };
        };
    }
  );
}
