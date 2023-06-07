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
        type = with config.lib.stylix; types.oneOf [static animation video slideshow];
        #god damn it i wish nix had case statements
        default = let
          message = ''
              the image, polarity and base16Scheme options are deprecieated
              it is recommended to use the constructor based approach instead
          '';
          onlyWallpaper = lib.warn message config.lib.stylix.mkStaticImage {
            image = config.stylix.image;
            polarity = config.stylix.polarity;
          };
          onlyScheme = lib.warn message config.lib.stylix.mkStaticFill config.stylix.base16Scheme;
          bothSchemeAndWallpaper = lib.warn message config.lib.stylix.mkStaticImage {
            image = config.stylix.image;
            override = config.stylix.base16Scheme;
            polarity = config.stylix.polarity;
          }; 
        in if (config.stylix.image != null && config.stylix.base16Scheme != null) then bothSchemeAndWallpaper else 
        (if (config.stylix.image == null && config.stylix.base16Scheme != null) then onlyScheme else 
        (if (config.stylix.image != null && config.stylix.base16Scheme == null) then onlyWallpaper else (throw "you have not set a wallpaper or a scheme")));
        description = mdDoc ''
        Wallpaper image.

        This is set as the background of your desktop environment, if possible,
        and used to generate a colour scheme if you don't set one manually.
        '';
    };

    image = mkOption {
      type = with types; nullOr (coercedTo package toString path);
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

    base16Scheme = mkOption {
      description = mdDoc ''
        A scheme following the base16 standard.

        This can be a path to a file, a string of YAML, or an attribute set.
      '';
      type = with types; nullOr (oneOf [ path lines attrs null ]);
      default = null;
      defaultText = literalMD ''
        The colors used in the theming.

        Those are automatically selected from the background image by default,
        but could be overridden manually.
      '';
    };
  };

  config = {
    # This attrset can be used like a function too, see
    # https://github.com/SenchoPens/base16.nix#mktheme
    #lib.stylix.colors = (base16.mkSchemeAttrs cfg.wallpaper.generatedColorScheme.palette).override override;
    lib.stylix.scheme = base16.mkSchemeAttrs cfg.wallpaper.generatedColorScheme.palette;
    lib.stylix.colors = base16.mkSchemeAttrs cfg.wallpaper.generatedColorScheme.palette;
  };
}
