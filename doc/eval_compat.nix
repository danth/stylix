{ lib, ... }:
{
  # Many modules assume they can do `options.programs ? foo`.
  # They should probably do `options ? programs.foo` instead,
  # but we can work around the issue with a stub option:
  options.programs.__stub = lib.mkSinkUndeclaredOptions { };

  # Third-party options are not included in the module eval,
  # so disable checking options definitions have matching declarations
  config._module.check = false;
}
