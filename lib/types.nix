{config, lib, ...}@args:
with lib;
{


  config.lib.stylix = {
    static = mkOptionType {
      name = "static";
      description = "static";
      descriptionClass = "static image";
    };

    animation = mkOptionType {
      name = "animation";
      description = "animation";
      descriptionClass = "non video animation";
    };

    video = mkOptionType {
      name = "video";
      description = "video";
      descriptionClass = "video";
    };

    slideshow = mkOptionType {
      name = "slideshow";
      description = "slideshow";
      descriptionClass = "collection of images";
    };
  };

  # boolean to check if object is type

  config.lib.stylix.isStatic = object: if (object.type == "static") then true else false;
  config.lib.stylix.isAnimation = object: if (object.type == "animation") then true else false;
  config.lib.stylix.isVideo = object: if (object.type == "video") then true else false;
  config.lib.stylix.isSlideshow = object: if (object.type == "slideshow") then true else false;
}
