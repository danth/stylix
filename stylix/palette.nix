{ palette-generator, base16 }:
{ pkgs, lib, config, ... }@args:

with lib;

let
  cfg = config.stylix;
  fromOs = import ./fromos.nix { inherit lib args; };
in
{
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
      type = with config.lib.stylix; types.oneOf [ static animation video slideshow ];
      #god damn it i wish nix had case statements
      default =
        let
          message = ''
            the image, polarity, overide and base16Scheme options are deprecieated
            it is recommended to use the constructor based approach instead which is documented on the stylix website
          '';
          onlyWallpaper = lib.warn message config.lib.stylix.mkStaticImage {
            image = config.stylix.image;
            polarity = config.stylix.polarity;
            override = config.stylix.override;
          };
          onlyScheme = lib.warn message config.lib.stylix.mkStaticFill {
              colorscheme = config.stylix.base16Scheme;
              override = config.stylix.override;
          };
        in
        fromOs [ "wallpaper" ] (if (config.stylix.image != null) then onlyWallpaper else
        (if (config.stylix.base16Scheme != null) then onlyScheme else (throw "the wallpaper option or one of the legacy methods was not set")));
      description = mdDoc ''
        Wallpaper image.

        This is set as the background of your desktop environment, if possible,
        and used to generate a colour scheme if you don't set one manually.
      '';
    };

    image = mkOption {
      type = with types; nullOr (coercedTo package toString path);
      default = fromOs [ "image" ] null;
      description = mdDoc ''
        Outdated method to set the wallpaper image
      '';
    };

    polarity = mkOption {
      type = types.enum [ "either" "light" "dark" ];
      default = "either";
      description = mdDoc ''
        Outdated method to set polarity, see the wallpaper option instead
      '';
    };

    base16Scheme = mkOption {
      type = with types; nullOr (oneOf [ path lines attrs ]);
      default = fromOs [ "base16Scheme" ] null;
      description = mdDoc ''
        Outdated method to set base16 scheme, see the wallpaper option instead
      '';
    };

    override = mkOption {
      description = mdDoc ''
        Outdated, set this using the wallpaper option instead
        A scheme following the base16 standard.

        This can be a path to a file, a string of YAML, or an attribute set.
      '';
      type = with types; oneOf [ path lines attrs ];
      default = fromOs [ "base16Scheme" ] { };
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
    lib.stylix.scheme = cfg.wallpaper.colors;
    lib.stylix.colors = cfg.wallpaper.colors;
  };
}
