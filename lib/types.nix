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

  config.lib.stylix.isStatic = value: (builtins.attrNames value == [ "colors" "image" ]);
  config.lib.stylix.isAnimation = value: (builtins.attrNames value == [ "animation" "colors" "image" ]);
  config.lib.stylix.isVideo = value: (builtins.attrNames value == [ "colors" "image" "video" ]);
  config.lib.stylix.isSlideshow = value: (builtins.attrNames value == [ "colors" "delay" "image" "images" ]);
}
