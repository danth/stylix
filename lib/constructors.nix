{ palette-generator, base16 }:
{ config, lib, pkgs, ... }@args:

with lib;

let generatePalette =
  { image, polarity, override }:
  let
    palette-json = pkgs.runCommand "palette.json" { } ''
      ${palette-generator}/bin/palette-generator ${polarity} ${image} $out
    '';
    palette = (importJSON palette-json) // {
      author = "Stylix";
      scheme = "Stylix";
      slug = "stylix";
    };
  in
    (base16.mkSchemeAttrs palette).override override;

in {
  config.lib.stylix = {
    mkStaticImage =
      { image, polarity ? "either", override ? { } }:
      {
        inherit image;
        colors = generatePalette { inherit image polarity override; };
      };

    mkStaticFill =
      { colorscheme, override ? { } }:
      let
        colors = (base16.mkSchemeAttrs colorscheme).override override;
      in {
        image = config.lib.stylix.solid colors.base00;
        inherit colors;
      };

    mkAnimation =
      { animation, polarity ? "either", override ? { } }:
      let
        image = pkgs.runCommand "image.png" { } ''
          ${pkgs.ffmpeg}/bin/ffmpeg -i ${animation} -vf "select=eq(n\,0)" -q:v 3 -f image2 $out
        '';
      in {
        inherit image animation;
        colors = generatePalette { inherit image polarity override; };
      };

    mkVideo =
      { video, polarity ? "either", override ? { } }:
      let
        image = pkgs.runCommand "image.png" { } ''
          ${pkgs.ffmpeg}/bin/ffmpeg -i ${video} -vf "select=eq(n\,0)" -q:v 3 -f image2 $out
        '';
      in {
        inherit image video;
        colors = generatePalette { inherit image polarity override; };
      };

    mkSlideshow =
      { images, delay ? 300, polarity ? "either", override ? { } }:
      let
        image = builtins.elemAt images 0;
      in {
        inherit image images delay;
        colors = generatePalette { inherit image polarity override; };
      };
  };
}
