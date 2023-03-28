# Tips and tricks

## Adjusting the brightness and contrast of a background image

If you want to use a background image for your desktop but find it too bright or distracting, you can use the `imagemagick` package to dim the image, or adjust its brightness and contrast to suit your preference.

Here's an example Nix expression that takes an input image, applies a brightness/contrast adjustment to it, and saves the result as a new image file:

```nix
{ pkgs, ... }:

let
  inputImage = ./path/to/image.jpg;
  brightness = -30;
  contrast = 0;
  fillColor = "black"
in
{
  stylix.image = pkgs.runCommand "dimmed-background.png" { } ''
    ${pkgs.imagemagick}/bin/convert "${inputImage}" -brightness-contrast ${brightness},${contrast} -fill ${fillColor} $out
  '';
}
```
