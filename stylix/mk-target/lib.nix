{
  cfg,
  config,
  lib,
}:
rec {
  # Get the list of function de-structured argument names.
  functionArgNames =
    fn:
    lib.pipe fn [
      lib.functionArgs
      builtins.attrNames
    ];

  getStylixAttrs =
    fn:
    lib.genAttrs (functionArgNames fn) (
      arg:
      if arg == "cfg" then
        cfg
      else if arg == "colors" then
        config.lib.stylix.colors
      else
        config.stylix.${arg}
          or (throw "stylix: mkTarget expected one of `cfg`, `colors`, ${
            lib.concatMapStringsSep ", " (name: "`${name}`") (
              builtins.attrNames config.stylix
            )
          }, but got: ${arg}")
    );

  # Call the configuration function with its required Stylix arguments.
  mkConfig = fn: fn (getStylixAttrs fn);

  # Safeguard configuration functions when any of their arguments is
  # disabled, while non-function configurations are unguarded.
  mkConditionalConfig =
    c:
    if builtins.isFunction c then
      let
        allAttrsNonNull = lib.pipe c [
          getStylixAttrs
          builtins.attrValues
          (builtins.all (attr: attr != null))
        ];
      in
      lib.mkIf allAttrsNonNull (mkConfig c)
    else
      c;
}
