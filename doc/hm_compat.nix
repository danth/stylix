{ lib, ... }:
{
  options = {
    # Many modules assume they can do `options.services ? foo`.
    # They should probably do `options ? services.foo` instead,
    # but we can work around the issue with a stub option:
    services.__stub = lib.mkSinkUndeclaredOptions { };

    # FIXME: this option is used as `autoEnable` for `stylix.targets.swaylock.enable`
    home.stateVersion = lib.mkOption {
      type = lib.types.str;
      description = "stub for home-manager's `stateVersion` option";
      default = "22.11";
      visible = false;
    };
  };

  # Some modules use home-manager's `osConfig` arg
  config._module.args.osConfig = null;
}
