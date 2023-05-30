{ config, lib, ... }@args:
with lib;
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
    config.lib.stylix.mkStatic = image: {
        type = "static";
        image = image;
    };

    config.lib.stylix.mkAnimation = animation: {
        type = "animation";
        image = pkgs.runCommand "image" {} ''
          ffmpeg -i ${video} -vf "select=eq(n\,0)" -q:v 3 output_image.jpg
        '';
        animation = animation;
    };

    config.lib.stylix.mkVideo = video: {
        type = "video";
        image = pkgs.runCommand "image" {} ''
          ffmpeg -i ${video} -vf "select=eq(n\,0)" -q:v 3 output_image.jpg 
        '';
        video = video;
    };

    config.lib.stylix.mkSlideshow = listOfImages: {
        type = "slideshow";
        image = builtins.elemAt listOfImages 0;
        files = listOfImages;
        delay = 5;
    };
}
