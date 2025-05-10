# See docs/src/modules.md#mkTarget for full explanation
{
  # Used when generating options; `stylix.targets.${name}`
  name,

  # Used when generating enable option; `stylix.targets.${name}.enable` has a
  # description of "Whether to enable theming for ${humanName}."
  humanName,

  # Whether the target should be auto-enabled
  autoEnable ? true,

  # Attributes will be mapped to `stylix.targets.${name}.${option}`
  extraOptions ? { },

  # Function(s) should take the Stylix attributes that it needs (e.g. `colors`,
  # `image`, `fonts`) and `cfg` if necessary. This `cfg` is an alias for
  # `config.stylix.targets.${name}`. Function(s) should return an attribute set
  # to be merged into `config`. If any of the Stylix attributes needed by a/the
  # function are `null`, AKA unset, the configuration will not be merged.
  configElements ? [ ],

  # Same format as functions under `configElements`. Merged regardless of whether
  # requested Stylix options are `null`. Avoid use when possible.
  generalConfig ? null,
}:
# Note: we only return a plain module so that calling code can use module args, while using this as their module body
let
  module =
    { config, lib, ... }:
    let
      cfg = config.stylix.targets.${name};

      # Get the function's de-structured arg names as a list
      functionArgNames =
        fn:
        lib.pipe fn [
          lib.functionArgs
          builtins.attrNames
        ];

      # Get the Stylix attrs (option values) requested by the function's de-structured args
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
              or (throw "stylix: Stylix arg was requested by mk-target but does not exist: ${arg}")
        );

      # Call the config element function with the requested stylix attr values
      # Returns a config definition
      mkConfig = fn: fn (getStylixAttrs fn);

      # Wrap the config returned by the config element with the appropriate `mkIf` condition
      # If the config element is a function, call it using `mkConfig` (above)
      # Returns a config definition wrapped with `mkIf`
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
    in
    {
      options.stylix.targets.${name}.enable =
        config.lib.stylix.mkEnableTarget humanName autoEnable;

      config = lib.mkIf (config.stylix.enable && cfg.enable) (
        lib.mkMerge (
          lib.optional (generalConfig != null) (mkConfig generalConfig)
          ++ map mkConditionalConfig (lib.toList configElements)
        )
      );
    };
in
{
  imports = [
    {
      options.stylix.targets.${name} = extraOptions;
    }
    module
  ];
}
