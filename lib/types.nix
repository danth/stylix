{config, lib, ...}@args:
with lib;
{


  config.lib.stylix = {
    static = mkOptionType {
      name = "static";
      description = "Static Image Type";
      descriptionClass = "image";
    };

    animation = mkOptionType {
      name = "animation";
      description = "Animation Type";
      descriptionClass = "non video animation";
    };

    video = mkOptionType {
      name = "video";
      description = "Video Type supporting all of the formats that the animation type does not";
      descriptionClass = "video";
    };

    slideshow = mkOptionType {
      name = "slideshow";
      description = "slideshow Type";
      descriptionClass = "collection of images";
    };
  };

  # boolean to check if object is type

  config.lib.stylix.isStatic = object: if (object.type == "static") then true else false;
  config.lib.stylix.isAnimation = object: if (object.type == "animation") then true else false;
  config.lib.stylix.isVideo = object: if (object.type == "video") then true else false;
  config.lib.stylix.isSlideshow = object: if (object.type == "slideshow") then true else false;
}
