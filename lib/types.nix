{config, lib, ...}@args:
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

    animation = mkOptionType {
      name = "animation";
      description = "animation";
      descriptionClass = "non video animation";
      check = isAnimation;
    };

    video = mkOptionType {
      name = "video";
      description = "video";
      descriptionClass = "video";
      check = isVideo;
    };

    slideshow = mkOptionType {
      name = "slideshow";
      description = "slideshow";
      descriptionClass = "collection of images";
      check = isSlideshow;
    };
  };

  # boolean to check if object is type

  config.lib.stylix.isStatic = value: (value ? image && value ? colors);
  config.lib.stylix.isAnimation = value: (value ? image && value ? colors && value ? animation);
  config.lib.stylix.isVideo = value: (value ? image && value ? colors && value ? video);
  config.lib.stylix.isSlideshow = value: (value ? image && value ? colors && value ? imageDir && value ? delay);
}
