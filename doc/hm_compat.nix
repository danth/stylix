{ lib, ... }:
{
  # Many modules assume they can do `options.services ? foo`.
  # They should probably do `options ? services.foo` instead,
  # but we can work around the issue with a stub option:
  options.services.__stub = lib.mkSinkUndeclaredOptions { };

  # Some modules use home-manager's `osConfig` arg
  config._module.args.osConfig = null;
}
