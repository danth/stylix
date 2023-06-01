{ config, lib, ... }@args:
with lib;

let
  fromOs = import ./fromos.nix { inherit lib args; };

  cfg = config.stylix;

  paletteJSON = {polarity, image}: let
    generatedJSON = pkgs.runCommand "palette.json" { } ''
      ${palette-generator}/bin/palette-generator ${polarity} ${image} $out
    '';
    palette = importJSON generatedJSON;
    scheme = base16.mkSchemeAttrs palette;
    json = scheme {
      template = builtins.readFile ./palette.json.mustache;
      extension = ".json";
    };
  in json;
  generatedScheme = importJSON paletteJSON;
in
{
    config.lib.stylix.static = mkOptionType {
        name = "static";
        description = "Static Image Type";
    };


    config.lib.stylix.animation = mkOptionType {
        name = "animation";
        description = "Animation Type";
    };

    config.lib.stylix.video = mkOptionType {
        name = "video";
        description = "Video Type supporting all of the formats that the animation type does not";
    };

    config.lib.stylix.slideshow = mkOptionType {
        name = "slideshow";
        description = "slideshow Type";
    };


    # constructors for the wallpaper types
    config.lib.stylix.mkStatic = {image, polarity ? "dark", base16Scheme ? ""}: {
        type = "static";
        image = image;
        generatedColorScheme = {
          json = if (base16Scheme != "") then (paletteJSON polarity image) else (base16Scheme);
          palette = if (base16Scheme != "") then (importJSON (paletteJSON polarity image)) else (importJSON base16Scheme);
        };
    };

    config.lib.stylix.mkAnimation = {animation, polarity ? "dark", base16Scheme ? ""}: let
        image = pkgs.runCommand "image" {} ''
          ffmpeg -i ${video} -vf "select=eq(n\,0)" -q:v 3 output_image.jpg
        '';
    in {
        type = "animation";
        image = image;
        generatedColorScheme = {
          json = if (base16Scheme != "") then (paletteJSON polarity image) else (base16Scheme);
          palette = if (base16Scheme != "") then (importJSON (paletteJSON polarity image)) else (importJSON base16Scheme);
        };
        animation = animation;
    };

    config.lib.stylix.mkVideo = {video, polarity ? "dark", base16Scheme ? ""}: let
        image = pkgs.runCommand "image" {} ''
          ffmpeg -i ${video} -vf "select=eq(n\,0)" -q:v 3 output_image.jpg
        '';
    in {
        type = "video";
        image = image;
        video = video;
    };

    config.lib.stylix.mkSlideshow = {listOfImages, polarity ? "dark", base16Scheme ? "", delay ? 5}: let
      image = builtins.elemAt listOfImages 0;
    in {
        type = "slideshow";
        image = image;
        files = listOfImages;
        delay = delay;
    };
}
