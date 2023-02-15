# Stylix

Stylix is a NixOS module which applies the same color scheme, font and
wallpaper to a wide range of applications and desktop environments. It also
exports utilities for you to use the theme in custom parts of your configuration.

Stylix is built using [base16.nix](https://github.com/SenchoPens/base16.nix#readme),
a library which processes themes created for
[base16](https://github.com/chriskempson/base16#readme) or
[Tinted Theming](https://github.com/tinted-theming).

## Installation

### NixOS

You can install Stylix into your NixOS configuration using
[Flakes](https://nixos.wiki/wiki/Flakes). This will provide theming for system
level programs such as bootloaders, splash screens, and display managers.

```nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    stylix.url = "github:danth/stylix";
  };

  outputs = { nixpkgs, stylix, ... }: {
    nixosConfigurations."«hostname»" = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [ stylix.nixosModules.stylix ./configuration.nix ];
    };
  };
}
```
<small>Minimal `flake.nix` for a NixOS configuration.</small>

Many applications cannot be configured system wide, so Stylix will also need
[Home Manager](https://github.com/nix-community/home-manager) to be able to
change their settings within your home directory.

[Installing Home Manager as a NixOS module](https://nix-community.github.io/home-manager/index.html#sec-install-nixos-module)
is highly recommended if you don't use it already. This will combine it with
your existing configuration, so you don't need to run any extra commands when
you rebuild, and the theme you set in NixOS will automatically be used for Home
Manager too.

When Stylix is installed to a NixOS configuration, it will automatically set up
its Home Manager modules if it detects that Home Manager is available. You can
theoretically use it without installing Home Manager, however most features
will be unavailable.

### Home Manager

If you would prefer to use the standalone version of Home Manager, you can
install Stylix directly into your Home Manager configuration instead. This
could be useful if you are on a different Linux distribution, or a NixOS
machine which is managed by someone else.

```nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    stylix.url = "github:danth/stylix";
  };

  outputs = { nixpkgs, home-manager, stylix, ... }: {
    homeConfigurations."«username»" = home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      modules = [ stylix.nixosModules.stylix ./home.nix ];
    };
  };
}
```
<small>Minimal `flake.nix` for a Home Manager configuration.</small>

If you choose to use both NixOS and Home Manager but configure them separately,
you will need to copy the settings described below into both of your
configurations, as keeping them separate means that they cannot follow each
other automatically.

## Wallpaper

To start theming, you need to set a wallpaper image.

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

At this point you should be able to rebuild and have a reasonable color scheme
generated based on the image you chose.

## Color scheme

### Automatic color schemes

If you only set a wallpaper, Stylix will use a [genetic
algorithm](https://en.wikipedia.org/wiki/Genetic_algorithm) to create a color
scheme. The quality of these automatically generated schemes can vary, but more
colorful images tend to have better results.

You can force the generation of a light or dark theme using the polarity
option:

```nix
stylix.polarity = "dark";
```

After rebuilding, the full theme can be previewed in a web browser by
opening either of these files:

- `/etc/stylix/palette.html` if you are using NixOS, or
- `$XDG_CONFIG_HOME/stylix/palette.html` (by default
  `~/.config/stylix/palette.html`) if you are using home-manager
  without NixOS.

### Manual color schemes

If you would prefer to choose a color scheme, you can use anything from
[the Tinted Theming repository](https://github.com/tinted-theming/base16-schemes),
or another file following that format.

To use Tinted Theming, either add their repository to your Flake inputs, or
fetch it as follows:

```nix
let base16-schemes = pkgs.fetchFromGitHub {
  owner = "tinted-theming";
  repo = "base16-schemes";
  rev = "...";
  sha256 = "...";
};
```

Then set the following option to the path of the color scheme you would like to
use:

```nix
stylix.base16Scheme = "${base16-schemes}/gruvbox-dark-hard.yaml";
```

`base16Scheme` can also accept other formats as supported by
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

These can be changed as you like.

To make things look more uniform, you could replace the serif font with
the sans-serif font:

```nix
stylix.fonts.serif = config.stylix.fonts.sansSerif;
```

Or even choose monospace for everything:

```nix
stylix.fonts = {
  serif = config.stylix.fonts.monospace;
  sansSerif = config.stylix.fonts.monospace;
  emoji = config.stylix.fonts.monospace;
};
```

## Multi-user configurations

For those apps which are configured through Home Manager, Stylix allows you to
choose a different theme for each user. This can be done by setting the theme
within Home Manager for that user rather than at the system level.

By default, all users follow the system theme. This can be turned off by
setting `stylix.homeManagerIntegration.followSystem = false`, in which case you
must explicitly set a theme for each user. Setting that option is not required
just to be able to override an individual theme.

If you would like to disable all Home Manager activity for a user, you can set
`stylix.homeManagerIntegration.autoImport = false`, then manually import the
Home Manager module for the users for which it should be enabled.

Note that if the wallpaper image for a user is different to the rest of the
system, a separate theme will always be generated for them, even though their
`base16Scheme` option has not been overridden. If you want that user to follow
the system theme while having a different wallpaper, you will need to manually
copy the system theme into their configuration. (This behaviour is necessary as
otherwise it would be impossible to use a generated theme for a user while
having a manually created theme for the rest of the system.)

## Turning targets on and off

In Stylix terms, a target is anything which can have colors, fonts or a
wallpaper applied to it. Each module in this repository should correspond to a
target of the same name.

Each target has an option like `stylix.targets.«target».enable` to turn its
styling on or off. Normally, it's turned on automatically when the target is
installed. You can set `stylix.autoEnable = false` to opt out of this
behaviour, in which case you'll need to manually enable each target you want to
be styled.

Targets are different between Home Manager and NixOS, and sometimes available
in both cases. If both are available, it is always correct to enable both.

The Stylix website has a list of the available targets
[for NixOS](https://danth.github.io/stylix/options.html)
and
[for Home Manager](https://danth.github.io/stylix/options-hm.html)
respectively.
