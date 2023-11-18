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
          then config.lib.stylix.make.static {
            inherit image;
          }
          else config.lib.stylix.make.static {
            inherit image polarity;
          }
        ))) 
  ];

  options.stylix = {
    wallpaper = mkOption {
      type = config.lib.stylix.types.wallpaper;
      description = mdDoc ''
        This option defines the desktop wallpaper.

        The simplest choice is a static image:

        ```nix
        { config, ... }:
        {
          stylix.wallpaper = config.lib.stylix.make.static {
            image = ./path/to/image.png;
          };
        }
        ```

        Static wallpapers work everywhere.
        [This table](https://danth.github.io/stylix/wallpaper-support.html)
        shows which software supports the other wallpaper types.

        These include slideshows:

        ```nix
        { config, ... }:
        {
          stylix.wallpaper = config.lib.stylix.make.slideshow {
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
          stylix.wallpaper = config.lib.stylix.make.animation {
            animation = ./path/to/animation.gif;
          };
        }
        ````

        And videos:

        ```nix
        { config, ... }:
        {
          stylix.wallpaper = config.lib.stylix.make.video {
            video = ./path/to/video.mp4;
          };
        }
        ````

        For any of the examples above, you may also give a `polarity`.
        This affects whether the color scheme based on that wallpaper
        will be `light` or `dark`; by default it could be either.
        This is irrelevant if you plan to choose your own color scheme.

        ```nix
        { config, ... }:
        {
          stylix.wallpaper = config.lib.stylix.make.static {
            image = ./night-sky.jpg;
            polarity = "dark";
          };
        }
        ```
      '';
    };

    colors = mkOption {
      type = config.lib.stylix.types.overridableScheme;
      default = config.stylix.wallpaper.colors;
      defaultText = literalMD "generated scheme based on `stylix.wallpaper`";
      description = ''
        Color scheme to be used throughout the configuration.

        This option can accept:

        - A whole scheme as a file
        - A whole scheme as an attribute set:
          - All of `base00` to `base0F`
          - Optionally `scheme`, `author`, `description` or `slug`
        - An override as an attribute set:
          - Anything from `base00` to `base0F`, but not all of them
          - Optionally `scheme`, `author`, `description` or `slug`

        Popular files can be imported from
        [`base16-schemes`](https://github.com/tinted-theming/base16-schemes):

        ```nix
        { pkgs, ... }:
        {
          stylix.colors = "''${pkgs.base16-schemes}/share/themes/ayu-mirage.yaml";
        }
        ```

        To choose a scheme and override it from the same place, use `mkMerge`:

        ```nix
        { pkgs, lib, ... }:
        {
          stylix.colors = lib.mkMerge [
            "''${pkgs.base16-schemes}/share/themes/gruvbox-dark-hard.yaml"
            { base00 = "000000"; }
          ];
        }
        ```

        If you have multiple overrides, they could be applied in any order.
      '';
    };
  };

  config = {
    stylix.wallpaper =
      let default = fromOs [ "wallpaper" ] null;
      in mkIf (default != null) (mkDefault default);

    lib.stylix = {
      colors = lib.warn "`config.lib.stylix.colors' has been renamed to `config.stylix.colors'. Please update your configuration accordingly." config.stylix.colors;
      scheme = lib.warn "`config.lib.stylix.scheme' has been renamed to `config.stylix.colors'. Please update your configuration accordingly." config.stylix.colors;
    };
  };
}
