{ palette-generator, base16 }:
{ config, lib, pkgs, ... }@args:

with lib;

let
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

in {
  config.lib.stylix = {
    mkStaticImage =
      { image, polarity ? "either" }:
      {
        inherit image;
        colors = generatePalette { inherit image polarity; };
      };

    mkStaticFill =
      { colors }:
      let scheme = base16.mkSchemeAttrs colors;
      in {
        image = config.lib.stylix.solid scheme.base00;
        inherit colors;
      };

    mkAnimation =
      { animation, polarity ? "either" }:
      rec {
        inherit animation;
        image = extractFirstFrame animation;
        colors = generatePalette { inherit image polarity; };
      };

    mkVideo =
      { video, polarity ? "either" }:
      rec {
        inherit video;
        image = extractFirstFrame video;
        colors = generatePalette { inherit image polarity; };
      };

    mkSlideshow =
      { images, delay ? 300, polarity ? "either" }:
      rec {
        inherit images delay;
        image = builtins.elemAt images 0;
        colors = generatePalette { inherit image polarity; };
      };
  };
}
