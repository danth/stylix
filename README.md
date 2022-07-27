# Stylix

Stylix is a NixOS module which applies the same color scheme, font and
wallpaper to a wide range of applications and desktop environments.
In some cases, theming can be activated as early as the bootloader!

It also exports utilities for you to apply the theming to custom parts of your
configuration.

Stylix is built using [base16.nix](https://github.com/SenchoPens/base16.nix#readme),
a library which handles the generation of config files from templates provided by
the [base16](https://github.com/chriskempson/base16#readme) project.

## Installation

You can install Stylix using [Flakes](https://nixos.wiki/wiki/Flakes),
for example:

```nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    stylix.url = "github:danth/stylix";
  };

  outputs = { self, nixpkgs, home-manager, stylix }: {
    nixosConfigurations."<hostname>" = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        home-manager.nixosModules.home-manager
        stylix.nixosModules.stylix
      ];
    };
  };
}
```

Stylix relies on [Home Manager](https://github.com/nix-community/home-manager)
for a lot of its work, so that needs to be imported too.

## Binary cache

Stylix includes a Haskell program which generates color palettes.
To avoid compiling this from source, add the following settings:

```nix
nix.settings = {
  substituters = "https://danth.cachix.org";
  trusted-public-keys = "danth.cachix.org-1:wpodfSL7suXRc/rJDZZUptMa1t4MJ795hemRN0q84vI=";
};
```

## Wallpaper

To get started, you need to set a wallpaper image.

```nix
stylix.image = ./wallpaper.png;
```

The option accepts derivations as well as paths, so you can fetch a wallpaper
directly from the internet:

```nix
stylix.image = pkgs.fetchurl {
  url = "https://www.pixelstalk.net/wp-content/uploads/2016/05/Epic-Anime-Awesome-Wallpapers.jpg";
  sha256 = "enQo3wqhgf0FEPHj2coOCvo7DuZv+x5rL/WIo4qPI50=";
};
```

The wallpaper is the only option which is required!

## Color scheme

### Automatic color schemes

If you only set a wallpaper, Stylix will use a homemade
[genetic algorithm](https://en.wikipedia.org/wiki/Genetic_algorithm)
to choose a color scheme based on it. The quality of the generated palettes can
vary - but more colorful images tend to have better results.

You can force a light or dark theme using the polarity option:

```nix
stylix.polarity = "dark";
```

### Mixed color schemes

You can override part of the scheme by hand, perhaps to select your background
and text colors manually while keeping the genetic accent colors:

```nix
stylix.palette = {
  base00 = "000000";
  # ...
  base07 = "ffffff";
};
```

The `baseXX` names correspond to
[this table](https://github.com/chriskempson/base16/blob/main/styling.md#styling-guidelines).

### Manual color schemes

Alternatively, you can use a pre-made colorscheme from
[the base16 repository](https://github.com/base16-project/base16-schemes):

```nix
stylix.base16Scheme = "${base16-schemes}/gruvbox-dark-hard.yaml";
```

If you want anything more complex - like running an external palette generator
program - `base16Scheme` can accept any argument to
[`mkSchemeAttrs`](https://github.com/SenchoPens/base16.nix/blob/main/DOCUMENTATION.md#mkschemeattrs).

## Fonts

The default combination of fonts is:

```nix
stylix.fonts = {
  serif = {
    package = pkgs.dejavu_fonts;
    name = "DejaVu Serif";
  };

  sansSerif = {
    package = pkgs.dejavu_fonts;
    name = "DejaVu Sans";
  };

  monospace = {
    package = pkgs.dejavu_fonts;
    name = "DejaVu Sans Mono";
  };

  emoji = {
    package = pkgs.noto-fonts-emoji;
    name = "Noto Color Emoji";
  };
};
```

You might like to set `serif = sansSerif` for a more uniform look:

```nix
stylix.fonts = rec {
  serif = sansSerif;

  sansSerif = {
    package = pkgs.dejavu_fonts;
    name = "DejaVu Sans";
  };
};
```

Or even use your favorite monospace font for all of them!

All that really matters is that `monospace` is actually monospace, as using a
non-monospace font there will probably break your terminal.

## Turning targets on and off

In Stylix terms, a target is anything which can have colors, fonts or a
wallpaper applied to it. Each module in this repository should correspond to a
target of the same name.

Each target has an option like `stylix.targets.«target».enable` to turn its
styling on or off. Normally, it's turned on automatically when the target is
installed. You can set `stylix.autoEnable = false` to opt out of this
behaviour, in which case you'll need to manually enable each target you want to
be styled.
