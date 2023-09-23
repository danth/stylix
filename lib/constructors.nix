{ palette-generator, base16 }:
{ config, lib, pkgs, ... }@args:

with lib;

let generatePalette =
  { image, polarity }:
  # TODO: make base16.nix able to load this file directly, rather than importing it here
  let palette = pkgs.runCommand "palette.json" { } ''
    ${palette-generator}/bin/palette-generator ${polarity} ${image} $out
  '';
  in importJSON palette;

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
      let image = pkgs.runCommand "image.png" { } ''
        ${pkgs.ffmpeg}/bin/ffmpeg -i ${animation} -vf "select=eq(n\,0)" -q:v 3 -f image2 $out
      '';
      in {
        inherit image animation;
        colors = generatePalette { inherit image polarity; };
      };

    mkVideo =
      { video, polarity ? "either" }:
      let image = pkgs.runCommand "image.png" { } ''
        ${pkgs.ffmpeg}/bin/ffmpeg -i ${video} -vf "select=eq(n\,0)" -q:v 3 -f image2 $out
      '';
      in {
        inherit image video;
        colors = generatePalette { inherit image polarity; };
      };

    mkSlideshow =
      { images, delay ? 300, polarity ? "either" }:
      let image = builtins.elemAt images 0;
      in {
        inherit image images delay;
        colors = generatePalette { inherit image polarity; };
      };
  };
}
