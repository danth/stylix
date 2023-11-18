{ palette-generator, base16 }:
{ pkgs, config, lib, options, ... }:

with lib;

let
  # Exports the given constructor function as `config.lib.stylix.make.«name»`,
  # and generates a type `config.lib.stylix.types.«name»`. The function must
  # return an attribute set for this to work. The generated type can be used to
  # create options which accept the output of the function.
  objectModule =
    {
      # Name of the type in camelCase.
      name,
      # Name of the type in human readable format.
      description,
      # Used to add brackets around the description where needed.
      # "noun" - A simple noun phrase such as "positive integer"
      # "conjunction" - A phrase with an "or" connective
      # "composite" - A phrase with an "of" connective
      descriptionClass,
      # Constructor function.
      constructor
    }:
    {
      # The type of objects is tracked via the `_object` attribute.
      # We cannot use `_type` because that is already used by `mkIf`,
      # `mkMerge`, and `mkForce` from the standard library.
      make.${name} = args: constructor args // { _object = name; };

      types.${name} = mkOptionType {
        inherit name description descriptionClass;
        check = value:
          builtins.isAttrs value &&
          value?_object &&
          value._object == name;
      };
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
    name = "image";
    description = "image wallpaper";
    descriptionClass = "noun";
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
      types.oneOf [ image slideshow animation video ];
  }

  (let
    bases = [
      "base00" "base01" "base02" "base03"
      "base04" "base05" "base06" "base07"
      "base08" "base09" "base0A" "base0B"
      "base0C" "base0D" "base0E" "base0F"
    ];
    meta = [
      "scheme" "author" "description" "slug"
    ];

    hasOnlyPermittedAttrs = set:
      all (a: elem a (bases ++ meta)) (attrNames set);

    hasAllRequiredAttrs = set:
      all (a: elem a (attrNames set)) bases;

    isSchemeOrOverride = value:
      types.path.check value ||
      (builtins.isAttrs value && hasOnlyPermittedAttrs value);

    isScheme = value:
      types.path.check value ||
      (builtins.isAttrs value && hasAllRequiredAttrs value);

    merge = optionName: definitions:
      let
        values = catAttrs "value" definitions;

        partitioned = partition isScheme values;
        schemes = partitioned.right;
        overrides = partitioned.wrong;

        schemeDefinition =
          if length schemes < 1
          then
            # If only overrides were found, try looking at the option default for a scheme.
            if hasAttrByPath (optionName ++ [ "default" ]) options
            then getAttrFromPath (optionName ++ [ "default" ]) options
            else throw "At least one definition for `${showOption optionName}' must contain a whole scheme."
          else
            if length schemes > 1
            then throw "Only one definition for `${showOption optionName}' may contain a whole scheme."
            else elemAt schemes 0;

        scheme = base16.mkSchemeAttrs schemeDefinition;

      in foldr (o: s: s.override o) scheme overrides;

  in {
    types.scheme = mkOptionType {
      name = "scheme";
      description = "base16 scheme";
      descriptionClass = "noun";
      check = isSchemeOrOverride;
      inherit merge;
    };
  })
]
