{ palette-generator, base16 }:
{ pkgs, lib, options, ... }:

with lib;

let
  # A "black box" which can store multiple types and convert between them.
  #
  # This is used for wallpapers so that we can put in advanced types like
  # animations and videos, and pull them back out as still images when the more
  # advanced type isn't supported.
  boxType =
    {
      # Name of the type in camelCase.
      boxName,
      # Name of the type in human readable format.
      boxDescription,
      # Attribute set of each possible thing which could be inside the box.
      #
      # Each attribute is formatted as follows:
      #
      # type1 = {
      #   description = "type 1";
      #
      #   # Type of the stored value using lib.types
      #   type = ...;
      #
      #   # Conversion functions from this type to either another variety,
      #   # or something outside of the box. Missing conversions are okay.
      #   to = {
      #     type2 = value: doSomething;
      #     type3 = value: doSomethingElse;
      #   };
      # }
      varieties
    }:
    rec {
      # Used for options which accept anything stored in this box.
      type = mkOptionType {
        name = boxName;
        description = boxDescription;
        descriptionClass = "noun";
        check = value:
          isAttrs value
          && value?_box
          && value._box == boxName;
      };

      # Used to put values into the box by doing
      # «box type».from.type1 «value»
      from = mapAttrs (
        varietyName: variety: value:
        let
          # If the stored type is a submodule, then this will fill in
          # default values for any missing attributes.
          mergedValue = variety.type.merge [] [{
            inherit value;
            file = "";
          }];

          directConversions = mapAttrs
            (_: conversion: conversion mergedValue)
            variety.to;

          # A shortcut which allows writing
          # «wallpaper».as.colors
          # rather than
          # «wallpaper».as.image.as.colors
          # by automatically looking up the intermediate type.
          indirectConversions = pipe directConversions [
            attrValues
            (catAttrs "as")
            (foldl (a: b: a // b) {})
          ];

          possibleConversions =
            indirectConversions //
            directConversions //
            # Conversion from this type to itself.
            { ${varietyName} = value; };

          # Error values for all possible output types. These will be overridden
          # unless the conversion is not possible by any means.
          missingConversions = pipe varieties [
            attrValues
            (map (variety: attrNames variety.to))
            flatten
            (a: a ++ attrNames varieties)
            (a: genAttrs a (conversionName: throw "Conversion from ${varietyName} to ${conversionName} is not possible"))
          ];

          box = {
            _box = boxName;

            # To get a certain type out of the box regardless of what is inside,
            # we can write
            # «boxed value».as.«type»
            as = missingConversions // possibleConversions;

            # This is used to get values out of the box while doing something
            # different depending on the stored type.
            #
            # «boxed value».unpack {
            #   type1 = value: doSomething;
            #   type2 = value: doSomethingElse;
            # }
            #
            # If there is no function for the current type, then it will be
            # converted to one of the other types.
            unpack =
              functions:
              let
                conversions = intersectLists
                  (attrNames functions)
                  (attrNames possibleConversions);
                conversion = elemAt conversions 0;
              in
                if functions?${varietyName}
                then functions.${varietyName} value
                else
                  if length conversions > 0
                  then functions.${conversion} as.${conversion}
                  else throw "${varietyName} cannot be unpacked with any of the provided functions";
          };
        in
          if variety.type.check value
          then box
          else throw "Invalid value for ${variety.description}"
      ) varieties;
    };

  extractFirstFrame =
    input:
    pkgs.runCommand "first-frame.png" { } ''
      ${pkgs.ffmpeg}/bin/ffmpeg -i ${input} -vf 'select=eq(n\,0)' -vframes 1 $out
    '';

in {
  lib.stylix.types.wallpaper = boxType {
    boxName = "wallpaper";
    boxDescription = "wallpaper";
    varieties = {
      image = {
        description = "still image";

        type = types.submodule {
          options = {
            file = mkOption {
              type = with types; oneOf [ path package ];
            };

            polarity = mkOption {
              type = types.enum [ "light" "dark" "either" ];
              default = "either";
            };
          };
        };

        to.colors =
          { file, polarity }:
          # TODO: make base16.nix able to load this file directly, rather than importing it here
          let palette = pkgs.runCommand "palette.json" { } ''
            ${palette-generator}/bin/palette-generator ${polarity} ${file} $out
          '';
          in importJSON palette;
      };

      slideshow = {
        description = "slideshow";

        type = types.submodule {
          options = {
            files = mkOption {
              type = with types; nonEmptyListOf (oneOf [ path package ]);
            };

            delay = mkOption {
              type = types.int;
              default = 300;
            };

            polarity = mkOption {
              type = types.enum [ "light" "dark" "either" ];
              default = "either";
            };
          };
        };

        to.image =
          { files, polarity, ... }:
          {
            file = elemAt files 0;
            inherit polarity;
          };
      };

      animation = {
        description = "animated image";

        type = types.submodule {
          options = {
            file = mkOption {
              type = with types; oneOf [ path package ];
            };

            polarity = mkOption {
              type = types.enum [ "light" "dark" "either" ];
              default = "either";
            };
          };
        };

        to.image =
          { file, polarity, ... }:
          {
            file = extractFirstFrame file;
            inherit polarity;
          };
      };

      video = {
        description = "video";

        type = types.submodule {
          options = {
            file = mkOption {
              type = with types; oneOf [ path package ];
            };

            polarity = mkOption {
              type = types.enum [ "light" "dark" "either" ];
              default = "either";
            };
          };
        };

        to.image =
          { file, polarity, ... }:
          {
            file = extractFirstFrame file;
            inherit polarity;
          };
      };
    };
  };

  lib.stylix.types.scheme =
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

    in mkOptionType {
      name = "scheme";
      description = "base16 scheme";
      descriptionClass = "noun";
      check = isSchemeOrOverride;
      inherit merge;
    };
}
