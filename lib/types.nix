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
      isStatic = value: (builtins.attrNames value == [ "colors" "image" ]);

      animation = mkOptionType {
        name = "animation";
        description = "animation";
        descriptionClass = "animated image such as a GIF";
        check = isAnimation;
      };
      isAnimation = value: (builtins.attrNames value == [ "animation" "colors" "image" ]);

      video = mkOptionType {
        name = "video";
        description = "video";
        descriptionClass = "video";
        check = isVideo;
      };
      isVideo = value: (builtins.attrNames value == [ "colors" "image" "video" ]);

      slideshow = mkOptionType {
        name = "slideshow";
        description = "slideshow";
        descriptionClass = "collection of images";
        check = isSlideshow;
      };
      isSlideshow = value: (builtins.attrNames value == [ "colors" "delay" "image" "images" ]);
    };
}
