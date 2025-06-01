# Adding modules

## Development setup

Currently the easiest way to test Stylix is to use the new code in your actual
configuration.

You might find it useful to override Stylix' input flake reference on your
flake, from `github:nix-community/stylix` to
`git+file:/home/user/path/to/stylix`, so that you don't need to push changes to
GitHub during testing.

To do that, instead of editing your `flake.nix`, you can leverage `nix`'
`--override-input` parameter (which can also be supplied through their
frontends: `nixos-rebuild`, `nix-on-droid` and even `nh`). It allows you to
deploy your changes in one fell swoop, without having to update the lock file of
your flake every time you make an edit.

Just append `--override-input stylix git+file:/home/user/path/to/stylix` to your
standard `nix` (or `nix` frontend) incantation.

Nix only reads files which are tracked by Git, so you also need to `git add
«file»` after creating a new file.

## Module naming

Modules should be named like `modules/«name»/«platform».nix`. For example,
`modules/avizo/hm.nix` is a Home Manager module which themes Avizo.

The following platforms are supported:

- NixOS (`nixos`)
- Home Manager (`hm`)
- Nix-Darwin (`darwin`)
- Nix-on-Droid (`droid`)

Correctly named modules will be imported automatically.

Other files needed by the module can also be stored within the `modules/«name»`
folder, using any name which is not on the list above.

## Module template

Modules should be created using the `mkTarget` function whenever possible (see
the [`/stylix/mk-target.nix`](
https://github.com/danth/stylix/blob/-/stylix/mk-target.nix) in-source
documentation for more details):

```nix
{ config, lib, mkTarget ... }:
mkTarget {
  name = "«name»";
  humanName = "«human readable name»";

  configElements =
    { colors }:
    {
      programs.«name».theme.background = colors.base00;
    };
}
```

> [!IMPORTANT]
> The `mkTarget` argument is only available to modules imported by Stylix's
> [autoload system](https://github.com/danth/stylix/blob/-/stylix/autoload.nix),
> e.g., `modules/«target»/«platform».nix` modules.
>
> I.e., it is not available to normal modules imported via the `imports` list.

When the `mkTarget` function cannot be used, modules must manually replicate its
safeguarding behaviour:

```nix
{ config, lib, ... }:
{
  options.stylix.targets.«name».enable =
    config.lib.stylix.mkEnableTarget "«human readable name»" true;

  config =
    lib.mkIf (config.stylix.enable && config.stylix.targets.«name».enable)
      {
        programs.«name».backgroundColor = config.lib.stylix.colors.base00;
      };
}
```

> [!CAUTION]
> If not using `mkTarget`, you **must** check _both_ `config.stylix.enable`
> _and_ your target's own`enable` option before defining any config.
>
> In the above example this is done using
> `config = lib.mkIf (config.stylix.enable && config.stylix.targets.«name».enable)`.

The human readable name will be inserted into the following sentence:

> Whether to enable theming for «human readable name».

If your module will touch options outside of `programs.«name»` or
`services.«name»`, it should include an additional condition in `mkIf` to
prevent any effects when the target is not installed.

The boolean value after `mkEnableTarget` should be changed to `false` if one of
the following applies:

- The module requires further manual setup to work correctly.
- There is no reliable way to detect whether the target is installed, *and*
enabling it unconditionally would cause problems.

### Overlays

If your module is provided as an overlay it uses a special format, where config
is transparently passed to the platform (e.g. nixos) and overlay is a function
taking two arguments and returning an attrset:

```nix
{
  lib,
  config,
  ...
}:
{
  options.stylix.targets.«name».enable =
    config.lib.stylix.mkEnableTarget "«human readable name»" true;

  overlay =
    final: prev:
    lib.optionalAttrs
      (config.stylix.enable && config.stylix.targets.«name».enable)
      {
        «name» = prev.«name».overrideAttrs (oldAttrs: {

        });
      };
}
```

## How to apply colors

Refer to the [style guide](./styling.md) to see how colors are named, and where
to use each one.

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
    let
      configFile = config.lib.stylix.colors {
        template = ./config.toml.mustache;
        extension = ".toml";
      };
    in
    "${configFile}";
}
```

Setting options through an existing NixOS or Home Manager module is preferable
to generating whole files, since users will have the option of overriding things
individually.

Also note that reading generated files with `builtins.readFile` can be very slow
and should be avoided.

## How to apply other things

For everything else, like fonts and wallpapers, you can just take option values
directly from `config`. See the reference pages for a list of options.

## Metadata

Metadata is stored in `/modules/«module»/meta.nix`. The following attributes are
available under `meta`:

- `name`: required human-readable string name.

- `homepage`: homepage string or attribute set of homepage strings, depending
  on the number of homepages:

  - ```nix
    homepage = "https://github.com/nix-community/stylix";
    ```

  - ```nix
    homepage = {
      Nix = "https://github.com/NixOS/nix";
      Nixpkgs = "https://github.com/NixOS/nixpkgs";
    };
    ```

  The attribute names are used as hyperlink text and the attribute values are
  used as URLs.

- `maintainers`: required list of maintainers. See [Maintainers](#maintainers)
  section.

- `description`: optional markdown string for extra documentation.

### Maintainers

New modules must have at least one maintainer.

If you are not already listed in the Nixpkgs `/maintainers/maintainer-list.nix`
maintainer list, add yourself to `/stylix/maintainers.nix`.

Add yourself as a maintainer in one of the following ways, depending on the
number of maintainers:

- ```nix
  { lib, ... }:
  {
    maintainers = [ lib.maintainers.danth ];
  }
  ```

- ```nix
  { lib, ... }:
  {
    maintainers = with lib.maintainers; [
      danth
      naho
    ];
  }
  ```

The main responsibility of module maintainers is to update and fix their
modules.

## Documentation

Documentation for options is automatically generated. To improve the quality of
this documentation, ensure that any custom options created using `mkOption` are
given an appropriate `type` and a detailed `description`. This may use Markdown
syntax for formatting and links.

For modules needing more general documentation, add a `description` to
`modules/«module»/meta.nix`:

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

## Common Mistakes

### `home.activation` Scripts

Any script run by `home.activation` must be preceded by `run` if the script is
to produce any permanent changes. Without this `run` wrapper, the script is run
in dry-run mode.
