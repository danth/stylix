{lib, ...}@args:
with lib;
{


  lib = {
    static = mkOptionType {
      name = "static";
      description = "Static Image Type";
    };

    animation = mkOptionType {
      name = "animation";
      description = "Animation Type";
    };

    video = mkOptionType {
      name = "video";
      description = "Video Type supporting all of the formats that the animation type does not";
    };

    slideshow = mkOptionType {
      name = "slideshow";
      description = "slideshow Type";
    };
  };

  # boolean to check if object is type

  lib.isStatic = object: if (object.type == "static") then true else false;
  lib.isAnimation = object: if (object.type == "animation") then true else false;
  lib.isVideo = object: if (object.type == "video") then true else false;
  lib.isSlideshow = object: if (object.type == "slideshow") then true else false;
}
