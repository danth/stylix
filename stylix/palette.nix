{ palette-generator, base16 }:
{
  pkgs,
  lib,
  config,
  ...
}:

let
  cfg = config.stylix;

  paletteJSON = let
    generatedJSON = pkgs.runCommand "palette.json" { } ''
      ${palette-generator}/bin/palette-generator \
        "${cfg.polarity}" \
        ${lib.escapeShellArg "${cfg.image}"} \
        "$out"
    '';
    palette = lib.importJSON generatedJSON;
    scheme = base16.mkSchemeAttrs palette;
    json = scheme {
      template = ./palette.json.mustache;
      extension = ".json";
    };
  in json;
  generatedScheme = lib.importJSON paletteJSON;

in
{
  imports = [
    ./image-editors
    (lib.mkRemovedOptionModule [ "stylix" "palette" "base00" ] "Using stylix.palette to override scheme is not supported anymore")

    # Added: 2023-02-02
    (lib.mkRemovedOptionModule [ "stylix" "palette" "base01" ] "Using stylix.palette to override scheme is not supported anymore")

    # Added: 2023-02-02
    (lib.mkRemovedOptionModule [ "stylix" "palette" "base02" ] "Using stylix.palette to override scheme is not supported anymore")

    # Added: 2023-02-02
    (lib.mkRemovedOptionModule [ "stylix" "palette" "base03" ] "Using stylix.palette to override scheme is not supported anymore")

    # Added: 2023-02-02
    (lib.mkRemovedOptionModule [ "stylix" "palette" "base04" ] "Using stylix.palette to override scheme is not supported anymore")

    # Added: 2023-02-02
    (lib.mkRemovedOptionModule [ "stylix" "palette" "base05" ] "Using stylix.palette to override scheme is not supported anymore")

    # Added: 2023-02-02
    (lib.mkRemovedOptionModule [ "stylix" "palette" "base06" ] "Using stylix.palette to override scheme is not supported anymore")

    # Added: 2023-02-02
    (lib.mkRemovedOptionModule [ "stylix" "palette" "base07" ] "Using stylix.palette to override scheme is not supported anymore")

    # Added: 2023-02-02
    (lib.mkRemovedOptionModule [ "stylix" "palette" "base08" ] "Using stylix.palette to override scheme is not supported anymore")

    # Added: 2023-02-02
    (lib.mkRemovedOptionModule [ "stylix" "palette" "base09" ] "Using stylix.palette to override scheme is not supported anymore")

    # Added: 2023-02-02
    (lib.mkRemovedOptionModule [ "stylix" "palette" "base0A" ] "Using stylix.palette to override scheme is not supported anymore")

    # Added: 2023-02-02
    (lib.mkRemovedOptionModule [ "stylix" "palette" "base0B" ] "Using stylix.palette to override scheme is not supported anymore")

    # Added: 2023-02-02
    (lib.mkRemovedOptionModule [ "stylix" "palette" "base0C" ] "Using stylix.palette to override scheme is not supported anymore")

    # Added: 2023-02-02
    (lib.mkRemovedOptionModule [ "stylix" "palette" "base0D" ] "Using stylix.palette to override scheme is not supported anymore")

    # Added: 2023-02-02
    (lib.mkRemovedOptionModule [ "stylix" "palette" "base0E" ] "Using stylix.palette to override scheme is not supported anymore")

    # Added: 2023-02-02
    (lib.mkRemovedOptionModule [ "stylix" "palette" "base0F" ] "Using stylix.palette to override scheme is not supported anymore")
  ];

  options.stylix = {
    polarity = lib.mkOption {
      type = lib.types.enum [ "either" "light" "dark" ];
      default = "either";
      description = ''
        Use this option to force a light or dark theme.

        By default we will select whichever is ranked better by the genetic
        algorithm. This aims to get good contrast between the foreground and
        background, as well as some variety in the highlight colours.
      '';
    };

    image = lib.mkOption {
      type = with lib.types; coercedTo package toString path;
      description = ''
        Wallpaper image.

        This is set as the background of your desktop environment, if possible,
        and used to generate a colour scheme if you don't set one manually.
      '';
    };

    imageScalingMode = lib.mkOption {
      type = lib.types.enum [ "stretch" "fill" "fit" "center" "tile" ];
      default = "fill";
      description = ''
        Wallpaper scaling mode;

        This is the scaling mode your wallpaper image will use assuming it
        doesnt fix your monitor perfectly
      '';
    };
    imageEditor = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = ''
          Update Wallpaper by applying a lut filter to the image
        '';
      };
      method = lib.mkOption {
        type = with lib.types; functionTo (coercedTo package toString path);
        default = config.lib.stylix.imageEditors.lutgen;
        description = ''
          A function to edit the image, takes one argument (the image)
          and returns the resulting edited image
        '';
        example = ''
          config.stylix.imageEditor.method = config.lib.stylix.imageEditors.lutgen;
        '';
      };
    };

    generated = {
      json = lib.mkOption {
        type = lib.types.path;
        description = "The result of palette-generator.";
        readOnly = true;
        internal = true;
        default = paletteJSON;
      };

      palette = lib.mkOption {
        type = lib.types.attrs;
        description = "The imported json";
        readOnly = true;
        internal = true;
        default = generatedScheme;
      };

      fileTree = lib.mkOption {
        type = lib.types.raw;
        description = "The files storing the palettes in json and html.";
        readOnly = true;
        internal = true;
      };
      image = lib.mkOption {
        type = with lib.types; coercedTo package toString path;
        default = cfg.image;
        readOnly = true;
        internal = true;
        apply = img: if cfg.imageEditor.enable then cfg.imageEditor.method img else img;
      };
    };

    base16Scheme = lib.mkOption {
      description = ''
        A scheme following the base16 standard.

        This can be a path to a file, a string of YAML, or an attribute set.
      '';
      type = with lib.types; oneOf [ path lines attrs ];
      default = generatedScheme;
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
      default = {};
    };
  };

  config = {
    # This attrset can be used like a function too, see
    # https://github.com/SenchoPens/base16.nix/blob/b390e87cd404e65ab4d786666351f1292e89162a/README.md#theme-step-22
    lib.stylix.colors = (base16.mkSchemeAttrs cfg.base16Scheme).override cfg.override;
    lib.stylix.scheme = base16.mkSchemeAttrs cfg.base16Scheme;

    stylix.generated.fileTree = {
      # Making palette.json part of the system closure will protect it from
      # garbage collection, so future configurations can be evaluated without
      # having to generate the palette again. The generator is not kept, only
      # the palette which came from it, so this uses very little disk space.
      # The extra indirection should prevent the palette generator from running
      # when the theme is manually specified. generated.json is necessary in
      # the presence of overrides.
      "stylix/generated.json".source = config.lib.stylix.scheme {
        template = ./palette.json.mustache;
        extension = ".json";
      };

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
