# Wallpaper constructors

Moving wallpapers are only supported for certain environments, as shown in the table below.

|              | Static | Slideshow | Animation | Video |
|--------------|--------|-----------|-----------|-------|
| GNOME        | ✔️      | ✔️         | ❌         | ❌     |
| GRUB         | ✔️      | ❌         | ❌         | ❌     |
| LightDM      | ✔️      | ❌         | ❌         | ❌     |
| Sway         | ✔️      | ✔️         | ✔️         | ✔️     |
| Xmonad       | ✔️      | ✔️         | ✔️         | ✔️     |
| spectrwm     | ✔️      | ✔️         | ✔️         | ✔️     |
| i3           | ✔️      | ✔️         | ✔️         | ✔️     |
| herbstluftwm | ✔️      | ✔️         | ✔️         | ✔️     |
| bspwm        | ✔️      | ✔️         | ✔️         | ✔️     |

If an unsupported wallpaper is used, Stylix will fall back to a static image created
from the first frame of the wallpaper.

## mkStaticImage
takes a set containing an image, as well as optionally polarity and an override, which can either be a path to a base16 yaml file or and attrset.
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
  images = [
    ./path/to/image
    ./path/to/other/image
  ];
  delay = 300;
};
```
## mkAnimation
takes a set containing animation (gif or similar format), as well as optionally polarity and an override as an input.
```nix
stylix.wallpaper = config.lib.stylix.mkAnimation {
  animation = /path/to/animation.gif;
  override = {
    base00 = "ffffff";
  };
};
```
## mkVideo
takes a set containing a video (mp4 or similar format), as well as optionally polarity and an override as an input.
```nix
stylix.wallpaper = config.lib.stylix.mkVideo{
  video = /path/to/video.mp4;
};
```
