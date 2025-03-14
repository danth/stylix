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
- Nix-on-Droid (`droid`)

Correctly named modules will be imported automatically.

Other files needed by the module can also be stored within the
`modules/«name»` folder, using any name which is not on the list above.

## Module template

All modules should have an enable option created using `mkEnableTarget`.
This is similar to
[`mkEnableOption`](https://nix-community.github.io/docnix/reference/lib/options/lib-options-mkenableoption/)
from the standard library, however it integrates with
[`stylix.enable`](./options/nixos.md#stylixenable) and
[`stylix.autoEnable`](./options/nixos.md#stylixautoenable)
and generates more specific documentation.

A general format for modules is shown below.

```nix
{ config, lib, ... }:

{
  options.stylix.targets.«name».enable =
    config.lib.stylix.mkEnableTarget "«human readable name»" true;

  config = lib.mkIf (config.stylix.enable && config.stylix.targets.«name».enable) {
    programs.«name».backgroundColor = config.lib.stylix.colors.base00;
  };
}
```

The human readable name will be inserted into the following sentence:

> Whether to enable theming for «human readable name».

If your module will touch options outside of `programs.«name»` or `services.«name»`,
it should include an additional condition in `mkIf` to prevent any effects
when the target is not installed.

The boolean value after `mkEnableTarget` should be changed to `false` if
one of the following applies:

- The module requires further manual setup to work correctly.
- There is no reliable way to detect whether the target is installed, *and*
  enabling it unconditionally would cause problems.

## How to apply colors

Refer to the [style guide](./styling.md) to see how colors are named,
and where to use each one.

The colors are exported under `config.lib.stylix.colors`, which originates from
[`mkSchemeAttrs`](https://github.com/SenchoPens/base16.nix/blob/main/DOCUMENTATION.md#mkschemeattrs).

You can use the values directly:

```nix
{
  environment.variables.MY_APPLICATION_COLOR = config.lib.stylix.colors.base05;
}
```

Or you can create a [Mustache](http://mustache.github.io/) template and use
it as a function. This returns a derivation which builds the template.

```nix
{
  environment.variables.MY_APPLICATION_CONFIG_FILE =
    let configFile = config.lib.stylix.colors {
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

## How to apply other things

For everything else, like fonts and wallpapers, you can just take option values
directly from `config`. See the reference pages for a list of options.

## Maintainership

Every new module *must* have a maintainer defined in `modules/«module»/meta.nix`.
This is a statement that you are responsible for updating and fixing issues with
the module. If you are not in nixpkgs maintainers you will need to add yourself
to `stylix/maintainers.nix` with the format [documentated here](https://github.com/NixOS/nixpkgs/blob/master/maintainers/maintainer-list.nix#L1).
Once you are in either nixpkgs maintainers or `stylix/maintainers.nix` you can
add yourself as a maintainer in `modules/«module»/meta.nix` like this:

```nix
{ lib, ... }:
{
  maintainers = [ lib.maintainers.danth ];
  # or with multiple maintainer
  maintainers = with lib.maintainers; [ danth naho ];
}
```

## Documentation

Documentation for options is automatically generated. To improve the quality
of this documentation, ensure that any custom options created using `mkOption`
are given an appropriate `type` and a detailed `description`. This may use
Markdown syntax for formatting and links.

For modules needing more general documentation, create
`modules/«module»/README.md`:

```markdown
# Module Name

Consider describing which applications are themed by this module (if it's not
obvious from the module name), how the applications need to be installed for the
module to work correctly, which theming items are supported (colors, fonts,
wallpaper, ...), and any caveats the user should be aware of.
```

This will be inserted before the automatically generated list of options.

## Testbeds

Adding [testbeds](./testbeds.md) for new modules is encouraged, but not
mandatory.
