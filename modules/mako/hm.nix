{pkgs, config, lib, options, ... }:

with config.stylix.colors.withHashtag;
with config.stylix.fonts;
{
  options.stylix.targets.mako.enable =
    config.lib.stylix.mkEnableTarget "Mako" true;

  # Referenced https://github.com/stacyharper/base16-mako
  config = lib.optionalAttrs (options.services ? mako) (lib.mkIf config.stylix.targets.mako.enable {
    services.mako = {
      backgroundColor = base00
        + (config.lib.stylix.opacityToHex config.stylix.opacity.popups);
      borderColor = base0D;
      textColor = base05;
      progressColor = "over ${base02}";
      font = "${sansSerif.name} ${toString sizes.popups}";
      # I wish the mako hm module was like the dunst one
      extraConfig = ''
        [urgency=low]
        background-color=${base00}${
          config.lib.stylix.opacityToHex config.stylix.opacity.popups
        }
        border-color=${base0D}
        text-color=${base0A}

        [urgency=high]
        background-color=${base00}${
          config.lib.stylix.opacityToHex config.stylix.opacity.popups
        }
        border-color=${base0D}
        text-color=${base08}
      '';
    }; 
  });
}
