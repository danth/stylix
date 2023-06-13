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
  config.lib.stylix.mkStaticImage = { image, polarity ? "either", override ? { } }:
    let
      scheme = if (builtins.isAttrs override) then (override) else builtins.fromJSON override;
    in
    {
      image = image;
      colors = if (override != null) then (base16.mkSchemeAttrs (generateScheme polarity image)).override scheme else (base16.mkSchemeAttrs (generateScheme polarity image));
    };

  config.lib.stylix.mkStaticFill = {colorscheme, override ? {}}:
    let
      scheme = if (builtins.isAttrs colorscheme) then (colorscheme) else builtins.fromJSON colorscheme;
    in
    {
      image = config.lib.stylix.solid scheme.base00;
      colors = (base16.mkSchemeAttrs scheme).override override;
    };

  config.lib.stylix.mkAnimation = { animation, polarity ? "either", override ? null }:
    let
      image = pkgs.runCommand "image.png" { } ''
        ${pkgs.ffmpeg}/bin/ffmpeg -i ${animation} -vf "select=eq(n\,0)" -q:v 3 -f image2 $out
      '';
      scheme = if (builtins.isAttrs override) then (override) else builtins.fromJSON override;
    in
    {
      image = image;
      colors = if (override != null) then (base16.mkSchemeAttrs (generateScheme polarity image)).override scheme else (base16.mkSchemeAttrs (generateScheme polarity image));
      animation = animation;
    };

  config.lib.stylix.mkVideo = { video, polarity ? "either", override ? null }:
    let
      image = pkgs.runCommand "image.png" { } ''
        ${pkgs.ffmpeg}/bin/ffmpeg -i ${video} -vf "select=eq(n\,0)" -q:v 3 -f image2 $out
      '';
      scheme = if (builtins.isAttrs override) then (override) else builtins.fromJSON override;
    in
    {
      image = image;
      colors = base16.mkSchemeAttrs (if (override != null) then (generateScheme polarity image).override scheme else (generateScheme polarity image));
      video = video;
    };

  config.lib.stylix.mkSlideshow = { images, polarity ? "either", override ? null, delay ? 300 }:
    let
      image = builtins.elemAt images 0;
    in
    {
      image = image;
      colors = base16.mkSchemeAttrs (if (override != null) then (generateScheme polarity image).override override else (generateScheme polarity image));
      images = images;
      delay = delay;
    };
}
