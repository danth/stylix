{ palette-generator, base16 }:
{ pkgs, lib, config, ... }@args:

with lib;

let
  cfg = config.stylix;
  fromOs = import ./fromos.nix { inherit lib args; };
in
{
  imports = [
    (lib.mkRemovedOptionModule [ "stylix" "palette" "base00" ] "The palette option has been removed.")
    (lib.mkRemovedOptionModule [ "stylix" "palette" "base01" ] "The palette option has been removed.")
    (lib.mkRemovedOptionModule [ "stylix" "palette" "base02" ] "The palette option has been removed.")
    (lib.mkRemovedOptionModule [ "stylix" "palette" "base03" ] "The palette option has been removed.")
    (lib.mkRemovedOptionModule [ "stylix" "palette" "base04" ] "The palette option has been removed.")
    (lib.mkRemovedOptionModule [ "stylix" "palette" "base05" ] "The palette option has been removed.")
    (lib.mkRemovedOptionModule [ "stylix" "palette" "base06" ] "The palette option has been removed.")
    (lib.mkRemovedOptionModule [ "stylix" "palette" "base07" ] "The palette option has been removed.")
    (lib.mkRemovedOptionModule [ "stylix" "palette" "base08" ] "The palette option has been removed.")
    (lib.mkRemovedOptionModule [ "stylix" "palette" "base09" ] "The palette option has been removed.")
    (lib.mkRemovedOptionModule [ "stylix" "palette" "base0A" ] "The palette option has been removed.")
    (lib.mkRemovedOptionModule [ "stylix" "palette" "base0B" ] "The palette option has been removed.")
    (lib.mkRemovedOptionModule [ "stylix" "palette" "base0C" ] "The palette option has been removed.")
    (lib.mkRemovedOptionModule [ "stylix" "palette" "base0D" ] "The palette option has been removed.")
    (lib.mkRemovedOptionModule [ "stylix" "palette" "base0E" ] "The palette option has been removed.")
    (lib.mkRemovedOptionModule [ "stylix" "palette" "base0F" ] "The palette option has been removed.")
  ];

  options.stylix = {
    wallpaper = mkOption {
      type = with config.lib.stylix; types.oneOf [ static animation video slideshow ];

      default =
        let
          message = ''
            The image, polarity, override and base16Scheme options are deprecated.
            Please use the new options documented at
            https://danth.github.io/stylix/configuration.html#wallpaper
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
          default =
            if (config.stylix.image != null)
            then onlyWallpaper
            else if (config.stylix.base16Scheme != null)
            then onlyScheme
            else throw ''
              Please set a wallpaper following the instructions at
              https://danth.github.io/stylix/configuration.html#wallpaper
            '';
        in
        fromOs [ "wallpaper" ] default;

      description = mdDoc ''
        Object containing a desktop wallpaper and matching color scheme.

        This should be created using one of the constructors provided.
      '';
    };

    image = mkOption {
      type = with types; nullOr (coercedTo package toString path);
      default = fromOs [ "image" ] null;
      description = mdDoc "This option is deprecated.";
      visible = false;
    };

    polarity = mkOption {
      type = types.enum [ "either" "light" "dark" ];
      default = "either";
      description = mdDoc "This option is deprecated.";
      visible = false;
    };

    base16Scheme = mkOption {
      type = with types; nullOr (oneOf [ path lines attrs ]);
      default = fromOs [ "base16Scheme" ] null;
      description = mdDoc "This option is deprecated.";
      visible = false;
    };

    override = mkOption {
      type = with types; oneOf [ path lines attrs ];
      default = fromOs [ "base16Scheme" ] { };
      description = mdDoc "This option is deprecated.";
      visible = false;
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
