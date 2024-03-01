{ config, lib, ... }:

let
  inherit (lib) findFirst foldl' genAttrs hasSuffix last mkOption pipe removeSuffix toUpper splitString stringToCharacters;
  inherit (lib.types) submodule attrsOf;
  inherit (lib.types.ints) u8;
  inherit (builtins) attrNames elem elemAt filter hasAttr head listToAttrs mapAttrs substring stringLength;
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

  colorSubmodule = {
    options = {
      r = mkOption {
        type = u8;
      };
      g = mkOption {
        type = u8;
      };
      b = mkOption {
        type = u8;
      };
    };
  };

  swatchSubmodule = {
    options = {
      fg = mkOption {
        type = submodule colorSubmodule;
      };
      bg = mkOption {
        type = submodule colorSubmodule;
      };
      ol = mkOption {
        type = submodule colorSubmodule;
      };
    };
  };

  wrappedColor = color: rec {
    inherit (color) r g b;
    asRgbDec = "rgb(${toString r}, ${toString g}, ${toString b})";
    asRgbaDec = alphaDec: "rgba(${toString r}, ${toString g}, ${toString b}, ${toString alphaDec})";
    asHex = "${lib.toHexString r}${lib.toHexString g}${lib.toHexString b}";
    asHexWithHash = "#${asHex}";
    asHexAlpha = alphaHex: "${asHex}${alphaHex}";
    asHexAlphaWithHash = alphaHex: "#${asHex}${alphaHex}";
    asDecInt = r * 256 + g * 16 + b;
    asDecIntAlpha = decAlpha: asDecInt * 16 + decAlpha;
    newFactored = factor: wrappedColor {
      r = r * factor;
      g = g * factor;
      b = b * factor;
    };
    newBlended = toBlend: wrappedColor {
      r = ((r + toBlend.r) / 2);
      g = ((g + toBlend.g) / 2);
      b = ((b + toBlend.b) / 2);
    };
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
  };

  swatchGenerators = listToAttrs (map (swatchFile: {
    name = getSwatchName swatchFile;
    value = colors: import swatchFile {
      inherit colors lib config;
      inherit (config.stylix) polarity;
      mkSwatch = fg: bg: ol: { inherit fg bg ol; };
    };
  } ) findSwatchFiles);

  getSwatches = targetOrder: let
    foundTarget = findFirst (check: elem check (attrNames config.stylix.swatches)) "default" targetOrder;
  in genAttrs (attrNames swatchGenerators) (name:
    wrappedSwatch (config.stylix.swatches.${foundTarget}.${name})
  );
  
  convertBase16ToWrappedColors = base16Attrs: {
    base00 = hexToRgb base16Attrs.base00;
    base01 = hexToRgb base16Attrs.base01;
    base02 = hexToRgb base16Attrs.base02;
    base03 = hexToRgb base16Attrs.base03;
    base04 = hexToRgb base16Attrs.base04;
    base05 = hexToRgb base16Attrs.base05;
    base06 = hexToRgb base16Attrs.base06;
    base07 = hexToRgb base16Attrs.base07;
    base08 = hexToRgb base16Attrs.base08;
    base09 = hexToRgb base16Attrs.base09;
    base0A = hexToRgb base16Attrs.base0A;
    base0B = hexToRgb base16Attrs.base0B;
    base0C = hexToRgb base16Attrs.base0C;
    base0D = hexToRgb base16Attrs.base0D;
    base0E = hexToRgb base16Attrs.base0E;
    base0F = hexToRgb base16Attrs.base0F;
  };
  
  genSwatches = colors: mapAttrs (name: generator:
    generator colors
  ) swatchGenerators;
  
  genSwatchesFromBase16Attrs = colors: genSwatches (convertBase16ToWrappedColors colors);
in {
  config = {
    lib.stylix = {
        inherit swatchGenerators hexToRgb getSwatches genSwatches genSwatchesFromBase16Attrs;
    };
    stylix.swatches.default = (genSwatchesFromBase16Attrs (config.lib.stylix.colors));
  };

  options.stylix = {
    swatches = mkOption {
      type = attrsOf (submodule {
        options = genAttrs (attrNames swatchGenerators) (name: mkOption {
          type = submodule swatchSubmodule;
        });
      });
    };
  };
}
