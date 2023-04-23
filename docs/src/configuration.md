# Configuration

## Wallpaper

To start theming, you need to set a wallpaper image.

```nix
{
  stylix.image = ./wallpaper.png;
}
```

The option accepts derivations as well as paths, so you can fetch an image
directly from the internet:

```nix
{
  stylix.image = pkgs.fetchurl {
    url = "https://www.pixelstalk.net/wp-content/uploads/2016/05/Epic-Anime-Awesome-Wallpapers.jpg";
    sha256 = "enQo3wqhgf0FEPHj2coOCvo7DuZv+x5rL/WIo4qPI50=";
  };
}
```

## Color scheme

### Generated schemes

If you only set a wallpaper, Stylix will use a
[genetic algorithm](https://en.wikipedia.org/wiki/Genetic_algorithm)
to create a color scheme. The quality of these schemes can vary, but more
colorful images tend to have better results.

You can force a light or dark scheme using the polarity option:

```nix
{
  stylix.polarity = "dark";
}
```

The current scheme can be previewed in a web browser at either
[`/etc/stylix/palette.html`](file:///etc/stylix/palette.html) for NixOS, or
`~/.config/stylix/palette.html` for Home Manager.

### Handmade schemes

If you prefer a handmade color scheme, you can choose anything from
[the Tinted Theming repository](https://github.com/tinted-theming/base16-schemes):

```nix
{
  stylix.base16Scheme = "${pkgs.base16-schemes}/share/themes/gruvbox-dark-hard.yaml";
}
```

This option also accepts other files and formats supported by
[`mkSchemeAttrs`](https://github.com/SenchoPens/base16.nix/blob/main/DOCUMENTATION.md#mkschemeattrs).

### Overriding

For convenience, it is possible to override parts of `stylix.base16Scheme` using
`stylix.override`. Anything that
[base16.nix](https://github.com/SenchoPens/base16.nix) accepts as override is
valid.

When using both the Home Manager and NixOS modules, both the system overrides
and the user-provided one are used in the user configuration if
`stylix.base16Scheme` is not changed in the user config. If that is the case,
only the user override is used.

## Fonts

The default combination of fonts is:

```nix
{
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
}
```

These can be changed as you like.

To make things look more uniform, you could replace the serif font with
the sans-serif font:

```nix
{
  stylix.fonts.serif = config.stylix.fonts.sansSerif;
}
```

Or even choose monospace for everything:

```nix
{
  stylix.fonts = {
    serif = config.stylix.fonts.monospace;
    sansSerif = config.stylix.fonts.monospace;
    emoji = config.stylix.fonts.monospace;
  };
}
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
The reference pages have a list of targets for [NixOS](options/nixos.md) and
[Home Manager](options/hm.md) respectively.
