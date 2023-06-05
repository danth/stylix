{ palette-generator, base16 }:
{ config, lib, pkgs, ... }@args:
with lib;
let
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
  # constructors for the wallpaper types
  lib.mkStatic = { image, polarity ? "dark", base16Scheme ? null}: {
    type = "static";
    image = image;
    generatedColorScheme = {
      json = if (base16Scheme == null) then (paletteJSON polarity image) else base16Scheme;
      palette = if (base16Scheme == null) then (generateScheme polarity image) else (importJSON base16Scheme);
    };
  };

  lib.mkAnimation = { animation, polarity ? "dark", base16Scheme ? null}:
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

  lib.mkVideo = { video, polarity ? "dark", base16Scheme ? null }:
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

  lib.mkSlideshow = { imageDir, polarity ? "dark", base16Scheme ? null, delay ? 300 }:
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
