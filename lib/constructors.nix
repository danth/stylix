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
        template = builtins.readFile ../stylix/palette.json.mustache;
        extension = ".json";
      };
    in
    json;
    generateScheme = polarity: image: importJSON (paletteJSON polarity image);
in
{
  # constructors for the wallpaper types
  config.lib.stylix.mkStaticImage = { image, polarity ? "dark", override ? null}: let
    scheme = if (builtins.isAttrs override) then (override) else builtins.fromJSON override; 
    schemeJson = if (builtins.isAttrs override) then (builtins.toJSON override) else override; 
  in {
    type = "static";
    image = image;
    generatedColorScheme = {
      json = if (override != null) then schemeJson else (paletteJSON polarity image);
      palette =  if (override != null) then scheme else (generateScheme polarity image);
    };
  };

  #config.lib.stylix.mkStaticFill = colorScheme: let
  #  scheme = if (builtins.isAttrs colorScheme) then (colorScheme) else builtins.fromJSON colorScheme;
  #  schemeJson = if (builtins.isAttrs colorScheme) then (builtins.toJSON colorScheme) else colorScheme; 
  #in {
  #    type = "static";
  #    image = config.lib.stylix.pixel scheme.base00;
  #    generatedColorScheme = {
  #      json = schemeJson;
  #      palette =  scheme;
  #    };
  #};

  config.lib.stylix.mkAnimation = { animation, polarity ? "dark", override ? null}:
    let
      image = pkgs.runCommand "image.png" { } ''
        ${pkgs.ffmpeg}/bin/ffmpeg -i ${animation} -vf "select=eq(n\,0)" -q:v 3 -f image2 $out
      '';
    in
    {
      type = "animation";
      image = image;
      generatedColorScheme = {
        json = if (override == null) then (paletteJSON polarity image) else override;
        palette = if (override == null) then (generateScheme polarity image) else (importJSON override);
      };
      animation = animation;
    };

  config.lib.stylix.mkVideo = { video, polarity ? "dark", override ? null }:
    let
      image = pkgs.runCommand "image.png" { } ''
        ${pkgs.ffmpeg}/bin/ffmpeg -i ${video} -vf "select=eq(n\,0)" -q:v 3 -f image2 $out
      '';
    in
    {
      type = "video";
      image = image;
      generatedColorScheme = {
        json = if (override == null) then (paletteJSON polarity image) else override;
        palette = if (override == null) then (generateScheme polarity image) else (importJSON override);
      };
      video = video;
    };

  config.lib.stylix.mkSlideshow = { imageDir, polarity ? "dark", override ? null, delay ? 300 }:
    let
      image = imageDir + ("/" + (builtins.elemAt (builtins.attrNames (builtins.readDir imageDir)) 0));
    in
    {
      type = "slideshow";
      image = image;
      generatedColorScheme = {
        json = if (override == null) then (paletteJSON polarity image) else override;
        palette = if (override == null) then (generateScheme polarity image) else (importJSON override);
      };
      imageDir = imageDir;
      delay = delay;
    };
}
