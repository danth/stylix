{ pkgs, lib, config, ... }:

with lib;

let
  cfg = config.stylix;

  # TODO: This Python library should be included in Nixpkgs
  colorgram = with pkgs.python3Packages;
    buildPythonPackage rec {
      pname = "colorgram.py";
      version = "1.2.0";
      src = fetchPypi {
        inherit pname version;
        sha256 = "1gzxgcmg3ndra2j4dg73x8q9dw6b0akj474gxyyhfwnyz6jncxz7";
      };
      propagatedBuildInputs = [ pillow ];
    };
  colorgramPython = pkgs.python3.withPackages (ps: [ colorgram ]);

  # Pass the wallpaper and any manually selected colors to ./colors.py and
  # return a JSON file containing the generated colorscheme
  colorsJSON = pkgs.runCommand "stylix-colors" {
    colors = builtins.toJSON config.stylix.colors;
    passAsFile = [ "colors" ];
  } ''
    ${colorgramPython}/bin/python ${./colors.py} \
      ${cfg.image} < $colorsPath > $out
  '';

in {
  options.stylix.colors = genAttrs [
    "base00"
    "base01"
    "base02"
    "base03"
    "base04"
    "base05"
    "base06"
    "base07"
    "base08"
    "base09"
    "base0A"
    "base0B"
    "base0C"
    "base0D"
    "base0E"
    "base0F"
  ] (name:
    mkOption {
      description = "Hexadecimal color value for ${name}.";
      default = null;
      defaultText = "Automatically selected from the background image.";
      type = types.nullOr (types.strMatching "[0-9a-fA-F]{6}");
    });

  config.lib.stylix.colors = importJSON colorsJSON;
}
