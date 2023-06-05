{ palette-generator, base16 }:
{ pkgs, lib, config, ... }@args:

with lib;

let
  cfg = config.stylix;
in {
  # TODO link to doc on how to do instead
  imports = [
    (lib.mkRemovedOptionModule [ "stylix" "palette" "base00" ] "Using stylix.palette to override scheme is not supported anymore")
    (lib.mkRemovedOptionModule [ "stylix" "palette" "base01" ] "Using stylix.palette to override scheme is not supported anymore")
    (lib.mkRemovedOptionModule [ "stylix" "palette" "base02" ] "Using stylix.palette to override scheme is not supported anymore")
    (lib.mkRemovedOptionModule [ "stylix" "palette" "base03" ] "Using stylix.palette to override scheme is not supported anymore")
    (lib.mkRemovedOptionModule [ "stylix" "palette" "base04" ] "Using stylix.palette to override scheme is not supported anymore")
    (lib.mkRemovedOptionModule [ "stylix" "palette" "base05" ] "Using stylix.palette to override scheme is not supported anymore")
    (lib.mkRemovedOptionModule [ "stylix" "palette" "base06" ] "Using stylix.palette to override scheme is not supported anymore")
    (lib.mkRemovedOptionModule [ "stylix" "palette" "base07" ] "Using stylix.palette to override scheme is not supported anymore")
    (lib.mkRemovedOptionModule [ "stylix" "palette" "base08" ] "Using stylix.palette to override scheme is not supported anymore")
    (lib.mkRemovedOptionModule [ "stylix" "palette" "base09" ] "Using stylix.palette to override scheme is not supported anymore")
    (lib.mkRemovedOptionModule [ "stylix" "palette" "base0A" ] "Using stylix.palette to override scheme is not supported anymore")
    (lib.mkRemovedOptionModule [ "stylix" "palette" "base0B" ] "Using stylix.palette to override scheme is not supported anymore")
    (lib.mkRemovedOptionModule [ "stylix" "palette" "base0C" ] "Using stylix.palette to override scheme is not supported anymore")
    (lib.mkRemovedOptionModule [ "stylix" "palette" "base0D" ] "Using stylix.palette to override scheme is not supported anymore")
    (lib.mkRemovedOptionModule [ "stylix" "palette" "base0E" ] "Using stylix.palette to override scheme is not supported anymore")
    (lib.mkRemovedOptionModule [ "stylix" "palette" "base0F" ] "Using stylix.palette to override scheme is not supported anymore")
  ];

  options.stylix = {
    wallpaper = mkOption {
        type = let
          tps = config.lib.stylix;
        in types.oneOf [tps.static tps.animation tps.video tps.slideshow];
        default = if (config.stylix.image == null) then null else lib.warn "the image and polarity options are deprecieated" config.lib.stylix.mkStatic {
            image = config.stylix.image;
            polarity = config.stylix.polarity;
        };
        description = mdDoc ''
        Wallpaper image.

        This is set as the background of your desktop environment, if possible,
        and used to generate a colour scheme if you don't set one manually.
        '';
    };

    image = mkOption {
      type = with types; coercedTo package toString path;
      default = null;
      description = mdDoc ''
        Outdated method to set the wallpaper image
      '';
    };

    polarity = mkOption {
      type = types.enum [ "either" "light" "dark" ];
      default = "either";
      description = mdDoc ''
         OutDated method to set polarity
      '';
    };
  };

  options.image = mkOption { 
    type = with types; coercedTo package toString path;
    default = null;
    description = mdDoc ''
      Outdated method to set the wallpaper image
    '';
  };

  options.polarity = mkOption {
    type = types.enum [ "either" "light" "dark" ];
    default = "either";
    description = mdDoc ''
       OutDated method to set polarity
    '';
  };

  config = {
    # This attrset can be used like a function too, see
    # https://github.com/SenchoPens/base16.nix#mktheme
    #lib.stylix.colors = (base16.mkSchemeAttrs cfg.wallpaper.generatedColorScheme.palette).override override;
    lib.stylix.scheme = base16.mkSchemeAttrs cfg.wallpaper.generatedColorScheme.palette;
    lib.stylix.colors = base16.mkSchemeAttrs cfg.wallpaper.generatedColorScheme.palette;
  };
}
