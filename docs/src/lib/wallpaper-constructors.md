## mkStaticImage
takes an image, as well as optionally polarity and an override, which can either be a path to a base16 yaml file or and attrset.
example
```nix
stylix.wallpaper = config.lib.stylix.mkStaticImage {
  image = /path/to/image;
  polarity = "dark";
  override = /path/to/scheme.yml;
}
```
## mkStaticFill
takes a set containing a colorscheme as an input as well as optionally an override.
```nix
stylix.wallpaper = config.lib.stylix.mkStaticFill /path/to/scheme.yml;
```
## mkSlideshow
takes an image directory, polarity, override, and a delay rate in seconds as an input.
```nix
stylix.wallpaper = config.lib.stylix.mkSlideshow {
  imageDir = /path/to/dir;
  delay = 300;
};
```
## mkAnimation
takes an animation (gif or similar format), as well as polarity and an override as an input.
```nix
stylix.wallpaper = config.lib.stylix.mkAnimation {
  animation = /path/to/animation.gif;
  override = {
    base00 = "ffffff";
  };
};
```
## mkVideo
takes an video (mp4 or similar format), as well as polarity and an override as an input.
```nix
stylix.wallpaper = config.lib.stylix.mkAnimation {
  video = /path/to/video.mp4;
};
```
