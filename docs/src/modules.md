# Adding modules

## Development setup

Currently the easiest way to test Stylix is to use the new code in your
actual configuration.

You might find it useful to change the flake reference in your configuration
from `github:danth/stylix` to `git+file:/home/user/path/to/stylix`
so that you don't need to push your changes to GitHub during testing.

Then, remember to run `nix flake lock --update-input stylix` to refresh the
flake each time you make an edit.

Nix only reads files which are tracked by Git, so you also need to
`git add «file»` after creating a new file.

## Module naming

Modules should be named like `modules/«name»/«platform».nix`. For example,
`modules/avizo/hm.nix` is a Home Manager module which themes Avizo.

The following platforms are supported:

- NixOS (`nixos`)
- Home Manager (`hm`)
- Nix-Darwin (`darwin`)

Correctly named modules will be imported automatically.

Other files needed by the module can also be stored within the
`modules/«name»` folder, using any name which is not on the list above.

## Module template

All modules should have an enable option created using `mkEnableTarget`.
This is similar to
[`mkEnableOption`](https://nix-community.github.io/docnix/reference/lib/options/lib-options-mkenableoption/)
from the standard library, however it integrates with
[`stylix.autoEnable`](./options/nixos.md#stylixautoenable)
and generates more specific documentation.

A general format for modules is shown below.

```nix
{ config, lib, ... }:

{
  stylix.targets.«name».enable = config.lib.stylix.mkEnableTarget "«human readable name»";

  config = lib.mkIf config.stylix.targets.«name».enable {
  };
}
```

The human readable name must fit into the following sentence:

> Whether to style «human readable name».

## How to apply colors

Refer to the [style guide](./styling.md) to see how colors are named,
and where to use each one.

The colors are exported under `config.stylix.colors`. You can use the values
directly:

```nix
{
  some.program.color = config.stylix.colors.base05;
}
```

Or you can create a [Mustache](http://mustache.github.io/) template and use
it as a function. This returns a derivation which builds the template.

```nix
{
  some.program.configPath =
    let configFile = config.stylix.colors {
      template = ./config.toml.mustache;
      extension = ".toml";
    };
    in "${configFile}";
}
```

Setting options through an existing NixOS or Home Manager module is preferable
to generating whole files, since users will have the option of overriding things
individually.

Also note that reading generated files with `builtins.readFile` can be very
slow and should be avoided.

## How to apply wallpapers

If your module will only support image wallpapers, then you can use
the image as follows:

```nix
{
  some.program.wallpaper = config.stylix.wallpaper.as.image.file;
}
```

Otherwise, use `unpack`:

```nix
{
  some.program.wallpaperURIs = config.stylix.wallpaper.unpack {
    slideshow =
      { files, ... }:
      map (f: "file://${f}") files;

    image =
      { file, ... }:
      [ "file://${file}" ];
  };
}
```

This calls the corresponding function for the type of wallpaper which the user
selected, kind of like [pattern matching](https://stackoverflow.com/a/2225811).

It's essential that still images are handled correctly, since any other missing
types can be converted to that.

## How to apply other things

For everything else, like fonts, you can just take option values
directly from `config`. See the reference pages for a list of options.
