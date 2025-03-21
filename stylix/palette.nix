{
  pkgs,
  lib,
  config,
  options,
  ...
}:
let
  cfg = config.stylix;
  adjustLightness =
    rgbColorString: primaryScale:
    let
      stripped = builtins.replaceStrings [ "rgb(" ")" ] [ "" "" ] rgbColorString;
      values = builtins.split "," stripped;
      v1 = builtins.fromJSON (builtins.elemAt values 0);
      v2 = builtins.fromJSON (builtins.elemAt values 2);
      v3 = builtins.fromJSON (builtins.elemAt values 4);
      preLightness = (v1 + v2 + v3) / 3.0;
      adj =
        (preLightness / 255.0 * (1.0 - primaryScale) + primaryScale) / preLightness
        * 255.0;
      v1adj = lib.max (lib.min (v1 * adj) 255.0) 0.0;
      v2adj = lib.max (lib.min (v2 * adj) 255.0) 0.0;
      v3adj = lib.max (lib.min (v3 * adj) 255.0) 0.0;
      round =
        x:
        let
          floored = builtins.floor x;
          diff = x - floored;
        in
        if diff >= 0.5 then floored + 1 else floored;
    in
    toString (lib.strings.fixedWidthString 2 "0" (lib.toHexString (round v1adj)))
    + toString (lib.strings.fixedWidthString 2 "0" (lib.toHexString (round v2adj)))
    + toString (lib.strings.fixedWidthString 2 "0" (lib.toHexString (round v3adj)));
in
{
  imports = [
    (lib.mkRemovedOptionModule
      [
        "stylix"
        "polarity"
      ]
      "The stylix.polarity option has been removed as a new palette generation backend is now used. New options for this palette generation backend are available at stylix.themeGeneration. The new stylix.themeGeneration.polarity sets the generated theme polarity (\"dark\", \"light\" options) and the \"either\" option doesn't exist anymore. You have to explicitly choose what polarity you want. The new options, stylix.themeGeneration.scheme, stylix.themeGeneration.contrast, stylix.themeGeneration.primaryScale.light, and stylix.themeGeneration.primaryScale.dark, let you tweak the generated color scheme further if need be."
    )
  ];

  options.stylix = {
    themeGeneration = {
      scheme = lib.mkOption {
        type = lib.types.enum [
          "scheme-content"
          "scheme-expressive"
          "scheme-fidelity"
          "scheme-fruit-salad"
          "scheme-monochrome"
          "scheme-neutral"
          "scheme-rainbow"
          "scheme-tonal-spot"
        ];
        default = "scheme-tonal-spot";
        description = ''
          Use this option to select a color scheme type.

          By default we will select matugen's default color scheme type.
        '';
      };
      contrast = lib.mkOption {
        type = lib.types.float;
        default = 0.0;
        description = ''
          Use this option to change the generated color scheme's contrast.

          Value from -1 to 1. -1 represents minimum contrast,
          0 represents standard (i.e. the design as spec'd),
          and 1 represents maximum contrast.

          By default, 0 will be used.
        '';
      };

      primaryScale = {
        dark = lib.mkOption {
          type = lib.types.float;
          default = 0.0;
          description = ''
            Use this option to change the generated dark color scheme's contrast.

            Value from -1 to 1. -1 represents minimum contrast,
            0 represents standard (i.e. the design as spec'd),
            and 1 represents maximum contrast.

            By default, 0 will be used.
          '';
        };
        light = lib.mkOption {
          type = lib.types.float;
          default = 0.0;
          description = ''
            Use this option to change the generated light color scheme's contrast.

            Value from -1 to 1. -1 represents minimum contrast,
            0 represents standard (i.e. the design as spec'd),
            and 1 represents maximum contrast.

            By default, 0 will be used.
          '';
        };
      };

      polarity = lib.mkOption {
        type = lib.types.enum [
          "light"
          "dark"
        ];
        default = "dark";
        description = ''
          Use this option to force a light or dark theme.

          By default we will select whichever is ranked better by the genetic
          algorithm. This aims to get good contrast between the foreground and
          background, as well as some variety in the highlight colours.
        '';
      };
    };

    image = lib.mkOption {
      type = with lib.types; nullOr (coercedTo package toString path);
      description = ''
        Wallpaper image.

        This is set as the background of your desktop environment, if possible,
        and used to generate a colour scheme if you don't set one manually.
      '';
      default = null;
    };

    imageScalingMode = lib.mkOption {
      type = lib.types.enum [
        "stretch"
        "fill"
        "fit"
        "center"
        "tile"
      ];
      default = "fill";
      description = ''
        Scaling mode for the wallpaper image.

        `stretch`
        : Stretch the image to cover the screen.

        `fill`
        : Scale the image to fill the screen, potentially cropping it.

        `fit`
        : Scale the image to fit the screen without being cropped.

        `center`
        : Center the image without resizing it.

        `tile`
        : Tile the image to cover the screen.
      '';
    };

    generated = {
      json = lib.mkOption {
        type = lib.types.package;
        description = "The output file produced by the palette generator.";
        readOnly = true;
        internal = true;
        # This *must* be the derivation running the palette generator,
        # and not anything indirect such as filling a template, otherwise
        # the output of the palette generator will not be protected from
        # garbage collection.
        default = pkgs.runCommand "raw-palette.json" { } ''
          ${pkgs.matugen}/bin/matugen \
            --json rgb \
            --type ${cfg.themeGeneration.scheme} \
            --contrast ${lib.strings.floatToString cfg.themeGeneration.contrast} \
            --dry-run \
            image \
            "${cfg.image}" \
            > "$out"
        '';
      };

      palette = lib.mkOption {
        type = lib.types.attrs;
        description = "The palette generated by the palette generator.";
        readOnly = true;
        internal = true;
        default =
          let
            jsonData = lib.importJSON cfg.generated.json;
            colors =
              if cfg.themeGeneration.polarity == "light" then
                jsonData.colors.light
              else
                jsonData.colors.dark;
          in
          if cfg.themeGeneration.polarity == "light" then
            {
              base00 = adjustLightness colors.background cfg.themeGeneration.primaryScale.dark;
              base01 = adjustLightness colors.surface_container cfg.themeGeneration.primaryScale.dark;
              base02 = adjustLightness colors.surface_container_highest cfg.themeGeneration.primaryScale.dark;
              base03 = adjustLightness colors.outline cfg.themeGeneration.primaryScale.dark;
              base04 = adjustLightness colors.on_surface_variant cfg.themeGeneration.primaryScale.dark;
              base05 = adjustLightness colors.on_surface cfg.themeGeneration.primaryScale.dark;
              base06 = adjustLightness colors.on_secondary_fixed cfg.themeGeneration.primaryScale.dark;
              base07 = adjustLightness colors.on_primary_container cfg.themeGeneration.primaryScale.dark;
              base08 = adjustLightness colors.error cfg.themeGeneration.primaryScale.dark;
              base09 = adjustLightness colors.on_tertiary cfg.themeGeneration.primaryScale.dark;
              base0A = adjustLightness colors.on_secondary_container cfg.themeGeneration.primaryScale.dark;
              base0B = adjustLightness colors.on_secondary_fixed_variant cfg.themeGeneration.primaryScale.dark;
              base0C = adjustLightness colors.on_primary_fixed cfg.themeGeneration.primaryScale.dark;
              base0D = adjustLightness colors.surface_tint cfg.themeGeneration.primaryScale.dark;
              base0E = adjustLightness colors.on_tertiary_fixed cfg.themeGeneration.primaryScale.dark;
              base0F = adjustLightness colors.on_error_container cfg.themeGeneration.primaryScale.dark;
            }
          else
            {
              base00 = adjustLightness colors.background cfg.themeGeneration.primaryScale.dark;
              base01 = adjustLightness colors.surface_container cfg.themeGeneration.primaryScale.dark;
              base02 = adjustLightness colors.surface_container_highest cfg.themeGeneration.primaryScale.dark;
              base03 = adjustLightness colors.outline cfg.themeGeneration.primaryScale.dark;
              base04 = adjustLightness colors.on_surface_variant cfg.themeGeneration.primaryScale.dark;
              base05 = adjustLightness colors.on_surface cfg.themeGeneration.primaryScale.dark;
              base06 = adjustLightness colors.secondary_fixed cfg.themeGeneration.primaryScale.dark;
              base07 = adjustLightness colors.on_primary_container cfg.themeGeneration.primaryScale.dark;
              base08 = adjustLightness colors.error cfg.themeGeneration.primaryScale.dark;
              base09 = adjustLightness colors.tertiary cfg.themeGeneration.primaryScale.dark;
              base0A = adjustLightness colors.secondary cfg.themeGeneration.primaryScale.dark;
              base0B = adjustLightness colors.primary cfg.themeGeneration.primaryScale.dark;
              base0C = adjustLightness colors.primary_fixed cfg.themeGeneration.primaryScale.dark;
              base0D = adjustLightness colors.surface_tint cfg.themeGeneration.primaryScale.dark;
              base0E = adjustLightness colors.tertiary_fixed cfg.themeGeneration.primaryScale.dark;
              base0F = adjustLightness colors.on_error_container cfg.themeGeneration.primaryScale.dark;
            };
      };

      fileTree = lib.mkOption {
        type = lib.types.raw;
        description = "The files storing the palettes in json and html.";
        readOnly = true;
        internal = true;
      };
    };

    base16Scheme = lib.mkOption {
      description = ''
        A scheme following the base16 standard.

        This can be a path to a file, a string of YAML, or an attribute set.
      '';
      type =
        with lib.types;
        nullOr (oneOf [
          path
          lines
          attrs
        ]);
      # defaults to null when cfg.image is also null, in order to trigger the assertion below correctly
      default = if cfg.image == null then null else cfg.generated.palette;
      defaultText = lib.literalMD ''
        The colors used in the theming.

        Those are automatically selected from the background image by default,
        but could be overridden manually.
      '';
    };

    override = lib.mkOption {
      description = ''
        An override that will be applied to stylix.base16Scheme when generating
        config.lib.stylix.colors.

        Takes anything that a scheme generated by base16nix can take as argument
        to override.
      '';
      type = lib.types.attrs;
      default = { };
    };

    paletteGenerator = lib.mkOption {
      description = "The palette generator executable.";
      type = lib.types.package;
      internal = true;
      readOnly = true;
    };

    base16 = lib.mkOption {
      description = "The base16.nix library.";
      internal = true;
      readOnly = true;
    };

    inputs = lib.mkOption {
      description = "Inputs of the Stylix flake.";
      internal = true;
      readOnly = true;
    };
  };

  config = {
    # This attrset can be used like a function too, see
    # https://github.com/SenchoPens/base16.nix/blob/b390e87cd404e65ab4d786666351f1292e89162a/README.md#theme-step-22
    lib.stylix.colors = (cfg.base16.mkSchemeAttrs cfg.base16Scheme).override cfg.override;

    assertions = lib.mkIf cfg.enable [
      {
        assertion = !(cfg.image == null && cfg.base16Scheme == null);
        message = "One of `stylix.image` or `stylix.base16Scheme` must be set";
      }
    ];

    stylix.generated.fileTree = {
      # The raw output of the palette generator.
      "stylix/generated.json" = {
        # We import the generated palette during evaluation but don't make it
        # a dependency, which means the garbage collector is free to delete it
        # immediately. Future evaluations may need to download, compile, and
        # run the palette generator from scratch to recreate the same palette.
        #
        # To improve performance, we can make the generated file part of the
        # system, which protects it from garbage collection and so increases
        # the potential for reuse between evaluations.
        #
        # The palette generator executable is not affected, and can still be
        # cleaned up as usual, so the overhead on system size is less than a
        # kilobyte.
        source = cfg.generated.json;

        # Only do this when `base16Scheme` is still the option default, which
        # is when the generated palette is used. Depending on the file in other
        # cases would force the palette generator to run when we never read the
        # output.
        #
        # Controlling this by comparing against the default value with == would
        # also force the palette generator to run, as we would have to evaluate
        # the default value to check for equality. To work around this, we
        # check only the priority of the resolved value. The priority of option
        # defaults is 1500 [1], and any value less than this means the user has
        # changed the option.
        #
        # [1]: https://github.com/NixOS/nixpkgs/blob/5f30488d37f91fd41f0d40437621a8563a70b285/lib/modules.nix#L1063
        enable = options.stylix.base16Scheme.highestPrio >= 1500;
      };

      # The current palette, with overrides applied.
      "stylix/palette.json".source = config.lib.stylix.colors {
        template = ./palette.json.mustache;
        extension = ".json";
      };

      # We also provide a HTML version which is useful for viewing the colors
      # during development.
      "stylix/palette.html".source = config.lib.stylix.colors {
        template = ./palette.html.mustache;
        extension = ".html";
      };
    };
  };
}
