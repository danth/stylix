{ palette-generator, base16 }:
{ pkgs, lib, config, ... }:

with lib;

let
  cfg = config.stylix;

  paletteJSON = pkgs.runCommand "palette.json" { } ''
    ${palette-generator}/bin/palette-generator ${cfg.polarity} ${cfg.image} $out
  '';
  generatedPalette = importJSON paletteJSON;

  paletteOverrides = filterAttrs (_: color: color != null) cfg.palette;

  metadata = {
    author = "Stylix";
    scheme = "Stylix";
    slug = "stylix";
  };

  stylixPalette = generatedPalette // paletteOverrides // metadata;

in {
  options.stylix = {
    polarity = mkOption {
      type = types.enum [ "either" "light" "dark" ];
      default = "either";
      description = ''
        Use this option to force a light or dark theme.

        By default we will select whichever is ranked better by the genetic
        algorithm. This aims to get good contrast between the foreground and
        background, as well as some variety in the highlight colours.
      '';
    };

    image = mkOption {
      type = types.coercedTo types.package toString types.path;
      description = ''
        Wallpaper image. This is set as the background of your desktop
        environment, if possible, and additionally used as the Plymouth splash
        screen if that is enabled. Colours are automatically selected from the
        picture to generate the system colour scheme.
      '';
    };

    palette = genAttrs [
      "base00" "base01" "base02" "base03" "base04" "base05" "base06" "base07"
      "base08" "base09" "base0A" "base0B" "base0C" "base0D" "base0E" "base0F"
    ] (base: mkOption {
      description = ''
        Hexadecimal color value for ${base}.

        These options can be used to override a section of the automatically
        generated palette, while keeping other parts. For example, you can
        select the background and text colors by setting bases 00 to 07, while
        keeping the accent colors automatic.

        See <literal>stylix.base16Scheme</literal> for importing an entire
        scheme.
      '';
      default = null;
      defaultText = "Automatically selected from the background image.";
      type = types.nullOr (types.strMatching "[0-9a-fA-F]{6}");
    });

    base16Scheme = mkOption {
      description = ''
        A scheme following the base16 standard.

        This can be a path to a file, a string of YAML, or an attribute set.

        Setting this option will completely disable the automatic palette
        generator, and use these colors instead.
      '';
      default = null;
      type = with types; nullOr (oneOf [ path lines attrs ]);
    };
  };

  # This attrset can be used like a function too, see
  # https://github.com/SenchoPens/base16.nix#mktheme
  config.lib.stylix.colors =
    if cfg.base16Scheme != null
    then base16.mkSchemeAttrs cfg.base16Scheme
    else base16.mkSchemeAttrs stylixPalette;
}
