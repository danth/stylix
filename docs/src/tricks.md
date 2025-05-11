# Tips and tricks

## Adjusting the brightness and contrast of a background image

If you want to use a background image for your desktop but find it too bright or distracting, you can use the `imagemagick` package to dim the image, or adjust its brightness and contrast to suit your preference.

Here's an example Nix expression that takes an input image, applies a brightness/contrast adjustment to it, and saves the result as a new image file:

```nix
{ pkgs, lib, ... }:
let
  inputImage = ./path/to/image.jpg;
  brightness = -30;
  contrast = 0;
  fillColor = "black";
in
{
  stylix.image = pkgs.runCommand "dimmed-background.png" { } ''
    ${lib.getExe' pkgs.imagemagick "convert"} "${inputImage}" -brightness-contrast ${brightness},${contrast} -fill ${fillColor} $out
  '';
}
```

## Dynamic wallpaper generation based on selected theme

With imagemagick, you can also dynamically generate wallpapers based on the selected theme.
Similarly, you can use a template image and repaint it for the current theme.

```nix
{ pkgs, lib, ... }:
let
  theme = "${pkgs.base16-schemes}/share/themes/catppuccin-latte.yaml";
  wallpaper = pkgs.runCommand "image.png" { } ''
    COLOR=$(${lib.getExe pkgs.yq} -r .palette.base00 ${theme})
    ${lib.getExe pkgs.imagemagick} -size 1920x1080 xc:$COLOR $out
  '';
in
{
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

## Extending CSS options

When trying to extend an attrset option, the order does not matter because a
declaration can only exist once. This is not the case for an option with the
type of `lines` (most commonly `style` options in Home Manager). For these options,
the order does matter and Nix cannot guarantee that there aren't conflicting
definitions. Nix will still merge these options, but it will not warn you if
there are conflicting declaration. In order to get around this, you can make sure
Nix puts your CSS at the end - and thus prioritizes it - by using `lib.mkAfter`:

```nix
{ lib, ... }:
{
  programs.waybar = {
    enable = true;
    style = lib.mkAfter ''
      #workspaces button {
        background: @base01;
      }
    '';
  };
}
```
