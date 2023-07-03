{ config, lib, ... }@args:
with lib;
with config.lib.stylix;
{
  config.lib.stylix = {
    static = mkOptionType {
      name = "static";
      description = "static";
      descriptionClass = "static image";
      check = isStatic;
    };
    isStatic = value: let
      imageType = builtins.isNull value.image || builtins.isPath value.image;
      colorsType = builtins.isAttrs value.colors;
    in (builtins.attrNames value == [ "colors" "image" ]) && imageType && colorsType;

    animation = mkOptionType {
      name = "animation";
      description = "animation";
      descriptionClass = "animated image such as a GIF";
      check = isAnimation;
    };
    isAnimation = value: let
      imageType = builtins.isNull value.image || builtins.isPath value.image;
      animationType = builtins.isNull value.animation || builtins.isPath value.animation;
      colorsType = builtins.isAttrs value.colors;
    in (builtins.attrNames value == [ "animation" "colors" "image" ]) && imageType && animationType && colorsType;

    video = mkOptionType {
      name = "video";
      description = "video";
      descriptionClass = "video";
      check = isVideo;
    };
    isVideo = value: let
      imageType = builtins.isNull value.image || builtins.isPath value.image;
      videoType = builtins.isNull value.video || builtins.isPath value.video;
      colorsType = builtins.isAttrs value.colors;
    in (builtins.attrNames value == [ "colors" "image" "video" ]) && imageType && videoType && colorsType;

    slideshow = mkOptionType {
      name = "slideshow";
      description = "slideshow";
      descriptionClass = "collection of images";
      check = isSlideshow;
    };
    isSlideshow = value: let
      imageType = builtins.isNull value.image || builtins.isPath value.image;
      imagesType = builtins.isList value.images;
      colorsType = builtins.isAttrs value.colors;
      delayType = builtins.isInt value.delay;
    in (builtins.attrNames value == [ "colors" "delay" "image" "images" ]) && imageType && imagesType && colorsType && delayType;
  };
}
