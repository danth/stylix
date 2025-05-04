{ lib, ... }:
{
  options = {
    # Many modules assume they can do `options.programs ? foo`.
    # They should probably do `options ? programs.foo` instead,
    # but we can work around the issue with a stub option:
    programs.__stub = lib.mkSinkUndeclaredOptions { };

    # The config.lib option, as found in NixOS and home-manager.
    # Many option declarations depend on `config.lib.stylix`,
    # so we need to provide a `lib` option.
    lib = lib.mkOption {
      type = lib.types.attrsOf lib.types.attrs;
      description = ''
        This option allows modules to define helper functions, constants, etc.
      '';
      default = { };
      visible = false;
    };
  };

  # Third-party options are not included in the module eval,
  # so disable checking options definitions have matching declarations
  config._module.check = false;
}
