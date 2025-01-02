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

## Dynamic wallpaper generation based on selected theme

With imagemagick, you can also dynamically generate wallpapers based on the selected theme.
Similarly, you can use a template image and repaint it for the current theme.

```nix
{ pkgs, ... }:

let
  theme = "${pkgs.base16-schemes}/share/themes/catppuccin-latte.yaml";
  wallpaper = pkgs.runCommand "image.png" {} ''
        COLOR=$(${pkgs.yq}/bin/yq -r .base00 ${theme})
        COLOR="#"$COLOR
        ${pkgs.imagemagick}/bin/magick -size 1920x1080 xc:$COLOR $out
  '';
in {
  stylix = {
    image = wallpaper;
    base16Scheme = theme;
  };
}
```

Which is neatly implemented as a single function in `lib.stylix.pixel`:

```nix
{ pkgs, config, ... }:

{
  stylix = {
    image = config.lib.stylix.pixel "base0A";
    base16Scheme = "${pkgs.base16-schemes}/share/themes/catppuccin-latte.yaml";
  };
}
```

## Completely disabling some stylix targets

Nixpkgs module system sometimes works in non-intuitive ways, e.g. parts
of the configuration guarded by `lib.mkIf` are still being descended
into. This means that every **loaded** (and not enabled) module must
be compatible with others - in the sense that **every** option that is
mentioned in the disabled parts of the configuration still needs to be
defined somewhere.

Sometimes that can be a problem, when your particular configuration
diverges enough from what stylix expects. In that case you can try
stubbing all the missing options in your configuration.

Or in a much clearer fashion you can just disable offending stylix targets
by adding the following `disableModules` line next to importing stylix
itself:

```nix
imports = [ flake.inputs.stylix.nixosModules.stylix ];
disabledModules = [ "${flake.inputs.stylix}/modules/<some-module>/nixos.nix" ];
```
