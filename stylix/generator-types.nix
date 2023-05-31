{ config, lib, ... }@args:
with lib;

let
  fromOs = import ./fromos.nix { inherit lib args; };

  cfg = config.stylix;

  paletteJSON = let
    generatedJSON = pkgs.runCommand "palette.json" { } ''
      ${palette-generator}/bin/palette-generator ${cfg.wallpaper.polarity} ${cfg.wallpaper.image} $out
    '';
    palette = importJSON generatedJSON;
    scheme = base16.mkSchemeAttrs palette;
    json = scheme {
      template = builtins.readFile ./palette.json.mustache;
      extension = ".json";
    };
  in json;
  generatedScheme = importJSON paletteJSON;

  override =
    (if cfg.base16Scheme == fromOs [ "base16Scheme" ] {}
     then fromOs [ "override" ] {}
     else {}) 
    // cfg.override;
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
        polarity = polarity;
    };

    config.lib.stylix.mkAnimation = {animation, polarity ? "dark", base16Scheme ? ""}: {
        type = "animation";
        image = pkgs.runCommand "image" {} ''
          ffmpeg -i ${video} -vf "select=eq(n\,0)" -q:v 3 output_image.jpg
        '';
        animation = animation;
    };

    config.lib.stylix.mkVideo = {video, polarity ? "dark", base16Scheme ? ""}: {
        type = "video";
        image = pkgs.runCommand "image" {} ''
          ffmpeg -i ${video} -vf "select=eq(n\,0)" -q:v 3 output_image.jpg 
        '';
        video = video;
    };

    config.lib.stylix.mkSlideshow = {listOfImages, polarity ? "dark", base16Scheme ? "", delay ? 5}: {
        type = "slideshow";
        image = builtins.elemAt listOfImages 0;
        files = listOfImages;
        delay = delay;
    };
}
