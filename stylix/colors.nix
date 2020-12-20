{ pkgs, lib, config, ... }:

with lib;

let
  cfg = config.stylix;

  # TODO: This Python library should be included in Nixpkgs
  colorgram = with pkgs.python3Packages; buildPythonPackage rec {
    pname = "colorgram.py";
    version = "1.2.0";
    src = fetchPypi {
      inherit pname version;
      sha256 = "1gzxgcmg3ndra2j4dg73x8q9dw6b0akj474gxyyhfwnyz6jncxz7";
    };
    propagatedBuildInputs = [ pillow ];
  };
  colorgramPython = pkgs.python3.withPackages (ps: [ colorgram ]);

  # Pass the wallpaper to ./colors.py and return a JSON file containing the
  # generated colorscheme
  colorsJSON = pkgs.runCommand "stylix-colors" {}
    "${colorgramPython}/bin/python ${./colors.py} ${cfg.image} > $out";

in {
  config.lib.stylix.colors = importJSON colorsJSON;
}
