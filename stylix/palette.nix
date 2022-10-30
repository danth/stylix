{ palette-generator, base16 }:
{ pkgs, lib, config, ... }:

with lib;

let
  cfg = config.stylix;

  paletteJSON = pkgs.runCommand "palette.json" { } ''
    ${palette-generator}/bin/palette-generator ${cfg.polarity} ${cfg.image} $out
  '';
  generatedPalette = importJSON paletteJSON;

  generatedScheme = cfg.palette // {
    author = "Stylix";
    scheme = "Stylix";
    slug = "stylix";
  };

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
        Wallpaper image.

        This is set as the background of your desktop environment, if possible,
        and used to generate a colour scheme if you don't set one manually.
      '';
    };

    palette = genAttrs [
      "base00" "base01" "base02" "base03" "base04" "base05" "base06" "base07"
      "base08" "base09" "base0A" "base0B" "base0C" "base0D" "base0E" "base0F"
    ] (base: mkOption {
      description = ''
        Hexadecimal color value for ${base}.

        You can use this option to override single colors.
        See <literal>stylix.base16Scheme</literal> if you want to import a
        whole base16 scheme from a file.

        You should not read from this option to access the chosen colors - use
        <literal>lib.stylix.colors</literal> instead.
        If <literal>stylix.base16Scheme</literal> is set to an external file,
        those colors won't appear here.
      '';
      type = types.strMatching "[0-9a-fA-F]{6}";
      default = generatedPalette.${base};
      defaultText = literalDocBook "Automatically selected from the background image.";
    });

    base16Scheme = mkOption {
      description = ''
        A scheme following the base16 standard.

        This can be a path to a file, a string of YAML, or an attribute set.
      '';
      type = with types; oneOf [ path lines attrs ];
      default = generatedScheme;
      defaultText = literalDocBook ''
        The colors defined in <literal>stylix.palette</literal>.

        Those are automatically selected from the background image by default,
        but could be overridden manually.
      '';
    };
  };

  config = {
    # This attrset can be used like a function too, see
    # https://github.com/SenchoPens/base16.nix#mktheme
    lib.stylix.colors = base16.mkSchemeAttrs cfg.base16Scheme;

    environment.etc = {
      # Making palette.json part of the system closure will protect it from
      # garbage collection, so future configurations can be evaluated without
      # having to generate the palette again. The generator is not kept, only
      # the palette which came from it, so this uses very little disk space.
      "stylix/palette.json".source = mkIf (cfg.base16Scheme == generatedScheme) paletteJSON;

      # We also provide a HTML version which is useful for viewing the colors
      # during development.
      "stylix/palette.html".source = config.lib.stylix.colors {
        template = builtins.readFile ./palette.html.mustache;
        extension = ".html";
      };
    };
  };
}
