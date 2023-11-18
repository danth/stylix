{ palette-generator, base16 }:
{ pkgs, config, lib, options, ... }:

with lib;

let
  typeModule =
    args:
    {
      types.${args.name} = mkOptionType args;
    };

  objectModule =
    args:
    let typeArgs = builtins.removeAttrs args [ "constructor" ];
    in {
      make.${args.name} = args.constructor;
      types.${args.name} = mkOptionType typeArgs;
    };

  # The definition for `config.lib` only merges the first level of attrsets
  # automatically, so we have to merge `make` and `types` explicitly
  mergeModules = modules: {
    lib.stylix = foldl recursiveUpdate {} modules;
  };

  generatePalette =
    { image, polarity }:
    # TODO: make base16.nix able to load this file directly, rather than importing it here
    let palette = pkgs.runCommand "palette.json" { } ''
      ${palette-generator}/bin/palette-generator ${polarity} ${image} $out
    '';
    in importJSON palette;

  extractFirstFrame =
    input:
    pkgs.runCommand "first-frame.png" { } ''
      ${pkgs.ffmpeg}/bin/ffmpeg -i ${input} -vf 'select=eq(n\,0)' -vframes 1 $out
    '';

in mergeModules [
  (objectModule {
    name = "static";
    description = "static wallpaper";
    descriptionClass = "noun";
    check = value:
      builtins.attrNames value == [ "colors" "image" ];
    constructor =
      { image, polarity ? "either" }:
      {
        inherit image;
        colors = generatePalette { inherit image polarity; };
      };
  })

  (objectModule {
    name = "slideshow";
    description = "slideshow wallpaper";
    descriptionClass = "noun";
    check = value:
      builtins.attrNames value == [ "colors" "delay" "image" "images" ];
    constructor =
      { images, delay ? 300, polarity ? "either" }:
      rec {
        inherit images delay;
        image = builtins.elemAt images 0;
        colors = generatePalette { inherit image polarity; };
      };
  })

  (objectModule {
    name = "animation";
    description = "animated wallpaper";
    descriptionClass = "noun";
    check = value:
      builtins.attrNames value == [ "animation" "colors" "image" ];
    constructor =
      { animation, polarity ? "either" }:
      rec {
        inherit animation;
        image = extractFirstFrame animation;
        colors = generatePalette { inherit image polarity; };
      };
  })

  (objectModule {
    name = "video";
    description = "video wallpaper";
    descriptionClass = "noun";
    check = value:
      builtins.attrNames value == [ "colors" "image" "video" ];
    constructor =
      { video, polarity ? "either" }:
      rec {
        inherit video;
        image = extractFirstFrame video;
        colors = generatePalette { inherit image polarity; };
      };
  })

  {
    types.wallpaper =
      with config.lib.stylix.types;
      types.oneOf [ static slideshow animation video ];
  }

  (typeModule {
    name = "scheme";
    description = "base16 scheme";
    descriptionClass = "noun";
    check = value:
      let
        bases = [
          "base00" "base01" "base02" "base03"
          "base04" "base05" "base06" "base07"
          "base08" "base09" "base0A" "base0B"
          "base0C" "base0D" "base0E" "base0F"
        ];
        meta = [
          "scheme" "author" "description" "slug"
        ];
        requiredAttrs = bases;
        allowedAttrs = bases ++ meta;
      in
        types.path.check value ||
        (builtins.isAttrs value
          && all (a: elem a allowedAttrs) (builtins.attrNames value)
          && all (a: elem a (builtins.attrNames value)) requiredAttrs
        );
  })

  (typeModule {
    name = "override";
    description = "override for base16 scheme";
    descriptionClass = "noun";
    emptyValue.value = { };
    check = value:
      let allowedAttrs = [
        "base00" "base01" "base02" "base03"
        "base04" "base05" "base06" "base07"
        "base08" "base09" "base0A" "base0B"
        "base0C" "base0D" "base0E" "base0F"
        "scheme" "author" "description" "slug"
      ];
      in builtins.isAttrs value
        && all (a: elem a allowedAttrs) (builtins.attrNames value);
  })

  (typeModule {
    name = "overridableScheme";
    description = "base16 scheme with overrides";
    descriptionClass = "conjunction";
    emptyValue.value = { };
    check = value:
      config.lib.stylix.types.scheme.check value ||
      config.lib.stylix.types.override.check value;
    merge = optionName: definitions:
      let
        values = catAttrs "value" definitions;
        partitioned = partition config.lib.stylix.types.scheme.check values;
        scheme =
          if length partitioned.right < 1
          then
            # If only overrides were provided, try applying them to the option default.
            if hasAttrByPath (optionName ++ [ "default" ]) options
            then getAttrFromPath (optionName ++ [ "default" ]) options
            else throw "At least one definition for `${showOption optionName}' must be a whole scheme."
          else
            if length partitioned.right > 1
            then throw "Only one definition for `${showOption optionName}' may be a whole scheme."
            else elemAt partitioned.right 0;
      in foldr
        (o: s: s.override o)
        (base16.mkSchemeAttrs scheme)
        partitioned.wrong;
  })
]
