{ palette-generator, base16 }:
{ config, lib, pkgs, ... }@args:
with lib;

let
  fromOs = import ./fromos.nix { inherit lib args; };

  cfg = config.stylix;

  paletteJSON = polarity: image:
    let
      generatedJSON = pkgs.runCommand "palette.json" { } ''
        ${palette-generator}/bin/palette-generator ${polarity} ${image} $out
      '';
      palette = importJSON generatedJSON;
      scheme = base16.mkSchemeAttrs palette;
      json = scheme {
        template = builtins.readFile ./palette.json.mustache;
        extension = ".json";
      };
    in
    json;
    generateScheme = polarity: image: importJSON (paletteJSON polarity image);
in
{

  # Type Definitions
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

  # boolean to check if object is type

  config.lib.stylix.isStatic = object: if (object.type == "static") then true else false;
  config.lib.stylix.isAnimation = object: if (object.type == "animation") then true else false;
  config.lib.stylix.isVideo = object: if (object.type == "video") then true else false;
  config.lib.stylix.isSlideshow = object: if (object.type == "slideshow") then true else false;


  # constructors for the wallpaper types
  config.lib.stylix.mkStatic = { image, polarity ? "dark", base16Scheme ? null}: {
    type = "static";
    image = image;
    generatedColorScheme = {
      json = if (base16Scheme == null) then (paletteJSON polarity image) else base16Scheme;
      palette = if (base16Scheme == null) then (generateScheme polarity image) else (importJSON base16Scheme);
    };
  };

  config.lib.stylix.mkAnimation = { animation, polarity ? "dark", base16Scheme ? null}:
    let
      image = pkgs.runCommand "image" { } ''
        ${pkgs.ffmpeg}/bin/ffmpeg -i ${animation} -vf "select=eq(n\,0)" -q:v 3 -f image2 $out
      '';
    in
    {
      type = "animation";
      image = image;
      generatedColorScheme = {
        json = if (base16Scheme == null) then (paletteJSON polarity image) else base16Scheme;
        palette = if (base16Scheme == null) then (generateScheme polarity image) else (importJSON base16Scheme);
      };
      animation = animation;
    };

  config.lib.stylix.mkVideo = { video, polarity ? "dark", base16Scheme ? null }:
    let
      image = pkgs.runCommand "image.png" { } ''
        ${pkgs.ffmpeg}/bin/ffmpeg -i ${video} -vf "select=eq(n\,0)" -q:v 3 -f image2 $out
      '';
    in
    {
      type = "video";
      image = image;
      generatedColorScheme = {
        json = if (base16Scheme == null) then (paletteJSON polarity image) else base16Scheme;
        palette = if (base16Scheme == null) then (generateScheme polarity image) else (importJSON base16Scheme);
      };
      video = video;
    };

  config.lib.stylix.mkSlideshow = { imageDir, polarity ? "dark", base16Scheme ? null, delay ? 300 }:
    let
      image = imageDir + ("/" + (builtins.elemAt (builtins.attrNames (builtins.readDir imageDir)) 0));
    in
    {
      type = "slideshow";
      image = image;
      generatedColorScheme = {
        json = if (base16Scheme == null) then (paletteJSON polarity image) else base16Scheme;
        palette = if (base16Scheme == null) then (generateScheme polarity image) else (importJSON base16Scheme);
      };
      imageDir = imageDir;
      delay = delay;
    };
}
