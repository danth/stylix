{ palette-generator, base16 }:
{ pkgs, lib, config, ... }@args:

with lib;

let
  fromOs = import ./fromos.nix { inherit lib args; };

in {
  imports = [
    # Originally, the scheme was always automatically generated and options
    # like `stylix.palette.base00' would override individual colors.
    # That was removed, and instead `stylix.base16Scheme' would accept an
    # entire scheme as either as an attrset or YAML file.
    # Then `stylix.override' was added to change part of that scheme,
    # similar to `stylix.palette' but as a single option taking an attrset.
    # Now `stylix.colors' combines all of this functionality.
    (mkRenamedOptionModule [ "stylix" "palette" ] [ "stylix" "colors" ])
    (mkRenamedOptionModule [ "stylix" "base16Scheme" ] [ "stylix" "colors" ])
    (mkRenamedOptionModule [ "stylix" "override" ] [ "stylix" "colors" ])

    (mkMergedOptionModule
      [ [ "stylix" "image" ] [ "stylix" "polarity" ] ]
      [ "stylix" "wallpaper" ]
      (values:
        with values.stylix;
        mkIf (image != "_mkMergedOptionModule") (
          if polarity == "_mkMergedOptionModule"
          then config.lib.stylix.mkStaticImage {
            inherit image;
          }
          else config.lib.stylix.mkStaticImage {
            inherit image polarity;
          }
        ))) 
  ];

  options.stylix = {
    wallpaper = mkOption {
      type = with config.lib.stylix;
        types.oneOf [ static slideshow animation video ];

      description = mdDoc ''
        This option defines the desktop wallpaper.

        The easiest choice is a static image:

        ```nix
        { config, ... }:
        {
          stylix.wallpaper = config.lib.stylix.mkStaticImage {
            image = ./path/to/image.png;
          };
        }
        ```

        [This table](https://danth.github.io/stylix/wallpaper-support.html)
        shows which software is compatible with the more interesting choices.
        These include slideshows:

        ```nix
        { config, ... }:
        {
          stylix.wallpaper = config.lib.stylix.mkSlideshow {
            images = [
              ./path/to/image/1.webp
              ./path/to/image/2.jpg
            ];
            delay = 60;
          };
        }
        ```

        Animated images:

        ```nix
        { config, ... }:
        {
          stylix.wallpaper = config.lib.stylix.mkAnimation {
            animation = ./path/to/animation.gif;
          };
        }
        ````

        And videos:

        ```nix
        { config, ... }:
        {
          stylix.wallpaper = config.lib.stylix.mkVideo {
            video = ./path/to/video.mp4;
          };
        }
        ````

        For any of the examples above, you may also give a `polarity`. This
        locks down whether the wallpaper is considered to be `light` or `dark`
        by the color scheme generator, so you will get a `light` or `dark` theme
        by default. Polarity is not relevant if you are choosing your own color
        scheme.

        ```nix
        { config, ... }:
        {
          stylix.wallpaper = config.lib.stylix.mkStaticImage {
            image = ./night-sky.jpg;
            polarity = "dark";
          };
        }
        ```
      '';
    };

    colors = mkOption {
      type = config.lib.stylix.overridableScheme;
      default = config.stylix.wallpaper.colors;

      defaultText = literalMD ''
        A scheme generated using colors from `stylix.wallpaper`.
      '';

      description = ''
        Color scheme to be used throughout the configuration.

        This option accepts either a whole scheme, or an override which changes
        part of the scheme.

        A whole scheme can be either a path to a file, or an attribute set
        containing `base00` to `base0F`, and optionally `scheme`, `author`,
        `description` or `slug`. Popular schemes are available through
        `pkgs.base16-schemes`:

        ```nix
        { pkgs, ... }:
        {
          stylix.colors = "''${pkgs.base16-schemes}/share/themes/ayu-mirage.yaml";
        }
        ```

        An override is an attribute set containing some but not all of `base00`
        to `base0F`, `scheme`, `author`, `description` or `slug`. These values
        will replace those from the original color scheme. If you have multiple
        overrides, they may be applied in any order.

        To choose a scheme and override it in the same file, use `mkMerge`:

        ```nix
        { pkgs, lib, ... }:
        {
          stylix.colors = lib.mkMerge [
            "''${pkgs.base16-schemes}/share/themes/gruvbox-dark-hard.yaml"
            { base00 = "000000"; }
          ];
        }
        ```
      '';
    };
  };

  config = {
    stylix.wallpaper =
      let default = fromOs [ "wallpaper" ] null;
      in mkIf (default != null) (mkDefault default);

    lib.stylix.colors =
      lib.warn "`config.lib.stylix.colors' has been renamed to `config.stylix.colors'. Please update your configuration accordingly." config.stylix.colors;
  };
}
