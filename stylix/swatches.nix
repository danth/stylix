{ base16, ... }:
{ config, lib, ... }:

let

  inherit (lib) findFirst foldl' hasSuffix last literalMD max mdDoc min mkOption pipe removeSuffix splitString stringToCharacters toHexString toUpper;

  inherit (lib.types) attrs attrsOf lines nullOr oneOf path submodule;

  inherit (lib.types.numbers) between;

  inherit (builtins) attrNames elem elemAt filter hasAttr head listToAttrs mapAttrs substring stringLength typeOf;

  constrainU8 = num: min (max num 0) 255;

  hex01 = {
    "0" = 0;
    "1" = 1;
    "2" = 2;
    "3" = 3;
    "4" = 4;
    "5" = 5;
    "6" = 6;
    "7" = 7;
    "8" = 8;
    "9" = 9;
    "a" = 10;
    "b" = 11;
    "c" = 12;
    "d" = 13;
    "e" = 14;
    "f" = 15;
    "A" = 10;
    "B" = 11;
    "C" = 12;
    "D" = 13;
    "E" = 14;
    "F" = 15;
  };

  hex10 = {
    "0" = 0;
    "1" = 16;
    "2" = 32;
    "3" = 48;
    "4" = 64;
    "5" = 80;
    "6" = 96;
    "7" = 112;
    "8" = 128;
    "9" = 144;
    "a" = 160;
    "b" = 176;
    "c" = 192;
    "d" = 208;
    "e" = 224;
    "f" = 240;
    "A" = 160;
    "B" = 176;
    "C" = 192;
    "D" = 208;
    "E" = 224;
    "F" = 240;
  };

  to2HexStr = num: pipe num [
    toHexString
    (str: if num < 10 then "0" + str else str)
  ];

  genBaseAttrs = fn: listToAttrs (map (base: rec {
    name = "base${base}";
    value = fn name;
  }) [
    "00" "01" "02" "03" "04" "05" "06" "07"
    "08" "09" "0A" "0B" "0C" "0D" "0E" "0F"
  ]);

  hexToRgb = str: pipe str [
    (str: stringToCharacters str)
    (chars: {
      r = hex10.${elemAt chars 0} + hex01.${elemAt chars 1};
      g = hex10.${elemAt chars 2} + hex01.${elemAt chars 3};
      b = hex10.${elemAt chars 4} + hex01.${elemAt chars 5};
    })
  ];

  getSwatchName = path:
    pipe path [
      (toString)
      (removeSuffix ".nix")
      (splitString "/")
      (last)
      (splitString "-")
      (strs: foldl' (acc: str:
        acc + (if str != (head strs) then
          (toUpper (substring 0 1 str)) + (substring 1 (stringLength str) str)
          else str))
        "" strs)
    ];

  findSwatchFiles = (filter (hasSuffix ".nix") (lib.filesystem.listFilesRecursive ../swatches));

  swatchGenerators = listToAttrs (map (swatchFile: {
    name = getSwatchName swatchFile;
    value = swatchFile;
  }) findSwatchFiles);

  blendFactor = config.stylix.blendFactor;

  wrappedColor = color: rec {
    inherit (color) r g b;
    asRgbDec = "rgb(${toString r}, ${toString g}, ${toString b})";
    asRgbaDec = alphaDec: "rgba(${toString r}, ${toString g}, ${toString b}, ${toString alphaDec})";
    asHex = "${to2HexStr r}${to2HexStr g}${to2HexStr b}";
    asHexWithHash = "#${asHex}";
    asHexAlpha = alphaHex: "${asHex}${alphaHex}";
    asHexAlphaWithHash = alphaHex: "#${asHex}${alphaHex}";
    asDecInt = r * 256 + g * 16 + b;
    asDecIntAlpha = decAlpha: asDecInt * 16 + decAlpha;
    newFactored = factor: wrappedColor {
      r = constrainU8 (r * factor);
      g = constrainU8 (g * factor);
      b = constrainU8 (b * factor);
    };
    newBlended = toBlend: wrappedColor {
      r = constrainU8 ((r + toBlend.r) / 2);
      g = constrainU8 ((g + toBlend.g) / 2);
      b = constrainU8 ((b + toBlend.b) / 2);
    };
    newBrighter = newFactored (blendFactor + 1.0);
    newDarker = newFactored blendFactor;
  };

  wrappedSwatch = swatch: let
    wrapped = hasAttr "asHex" swatch.fg;
  in rec {
    fg = if wrapped then swatch.fg else wrappedColor swatch.fg;
    bg = if wrapped then swatch.bg else wrappedColor swatch.bg;
    ol = if wrapped then swatch.ol else wrappedColor swatch.ol;
    newFactored = factor: wrappedSwatch {
      fg = fg.newFactored factor;
      bg = bg.newFactored factor;
      ol = ol.newFactored factor;
    };
    newBlended = color: wrappedSwatch {
      fg = fg.newBlended color;
      bg = bg.newBlended color;
      ol = ol.newBlended color;
    };
    newBrighter = color: newFactored (blendFactor + 1.0);
    newDarker = color: newFactored blendFactor;
  };

  getSwatches = targetOrder: let
      type = typeOf targetOrder;
      target = if type == "list" then
        findFirst (check: elem check (attrNames config.stylix.swatches)) "default" targetOrder
      else if type == "string" then
        if hasAttr targetOrder config.stylix.swatches then
          targetOrder
        else
          "default"
      else
        "default";
    in
      getSwatchesFromOpts target;

  getSwatchesFromOpts = target: let
    opts = config.stylix.swatches.${target};
    override = opts.override;
    scheme = if opts.base16Scheme == null then
      config.stylix.base16Scheme
    else
      opts.base16Scheme;
    colors = if (findFirst (hasSuffix "base") null (attrNames override)) != null then
          if scheme == config.stylix.base16Scheme then
            config.lib.stylix.colors
          else
            base16.mkSchemeAttrs scheme
        else
          (base16.mkSchemeAttrs scheme).override override;
    args = {
      inherit lib config;
      inherit (config.stylix) polarity;
      colors = genBaseAttrs (base: hexToRgb colors.${base});
      mkSwatch = fg: bg: ol: wrappedSwatch { inherit fg bg ol; };
    };
  in (mapAttrs (name: genFile:
    if (elem name (attrNames override)) then
      override.${name} args
    else
      import genFile args
  ) swatchGenerators) // colors;

in {
  config = {
    lib.stylix = {
      inherit
        swatchGenerators
        hexToRgb
        getSwatches;
    };
    stylix.swatches.default = {};
  };

  options.stylix = {
    swatches = mkOption {
      type = attrsOf (submodule {
        options = {
          base16Scheme = mkOption {
            description = mdDoc ''
              The scheme for the swatches.

              This can be a path to a file, a string of YAML, or an attribute set.
            '';
            type = nullOr (oneOf [ path lines attrs ]);
            default = null;
            defaultText = literalMD ''
              The colors used in the theming.

              Those are automatically selected from the background image by default,
              but could be overridden manually.
            '';
          };
          override = mkOption {
            description = mdDoc ''
              An override that will be both be applied to the swatches' base16Scheme when generating
              the $'{swatch}.colors,

              Takes anything that a scheme generated by base16nix can take as argument
              to override.
            '';
            type = attrs;
            default = {};
          };
        };
      });
      description = "Attr set containing base16Schemes and overrides to build swatches from";
    };
    colors.blendFactor = lib.mkOption {
      type = between 0 1;
      description = mdDoc "Default amount to blend incremental highlights/color changes by. Range of 0.0 - 1.0.";
      default = 0.2;
    };
  };
}
