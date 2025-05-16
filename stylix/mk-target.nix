/**
  Provides a consistent target interface, minimizing boilerplate and
  automatically safeguarding declarations related to disabled options.

  # Type

  ```
  mkTarget :: AttrSet -> ModuleBody
  ```

  # Examples

  This function returns the module body, not an actual module.

  The `modules/«MODULE»/«PLATFORM».nix` modules should use this function as
  follows:

      { mkTarget, lib... }:
      mkTarget {
        name = "«name»";
        humanName = "«human readable name»";

        generalConfig =
          lib.mkIf complexCondition {
            home.packages = [ pkgs.hello ];
          };

        configElements = [
          { programs.«name».theme.name = "stylix"; }

          (
            { colors }:
            {
              programs.«name».theme.background = colors.base00;
            }
          )

          (
            { fonts }:
            {
              programs.«name».font.name = fonts.monospace.name;
            }
          )
        ];
      }

  Note that `mkTarget` is provided in the same place as module arguments.

  Ideally, in the future, this function returns an actual module by better
  integrating with the /stylix/autoload.nix logic, allowing the following target
  simplification and preventing access to unguarded module arguments by
  requiring /modules/<MODULE>/<PLATFORM>.nix files to be attribute sets instead
  of modules:

      {
        name = "«name»";
        humanName = "«human readable name»";

        generalConfig =
          { lib, pkgs }:
          lib.mkIf complexCondition {
            home.packages = [ pkgs.hello ];
          };

        configElements = [
          { programs.«name».theme.name = "stylix"; }

          (
            { colors }:
            {
              programs.«name».theme.background = colors.base00;
            }
          )

          (
            { fonts }:
            {
              programs.«name».font.name = fonts.monospace.name;
            }
          )
        ];
      }

  # Inputs

  : `name`
    : The target name used to generate options in the stylix.targets.${name}
      namespace.

  : `humanName`
    : The descriptive target name passed to the lib.mkEnableOption function
      when generating the stylix.targets.${name}.enable option.

  : `autoEnable`
    : Whether the target should be automatically enabled by default according
      to the stylix.autoEnable option.

      This should be disabled if manual setup is required or if auto-enabling\
      causes issues.

  : `extraOptions`
    : Additional options to be added in the stylix.targets.${name} namespace
      along the stylix.targets.${name}.enable option.

      For example, an extension guard used in the configuration can be declared
      as follows:
      { extension.enable = lib.mkEnableOption "the bloated dependency"; }

  : `configElements`
    : Configuration functions that are automatically safeguarded when any of
      their arguments is disabled. The provided 'cfg' argument conveniently
      aliases to config.stylix.targets.${name}.

      For example, the following configuration is not merged if the stylix
      colors option is disabled:

          (
            { colors }:
            {
              programs.«name».theme.background = colors.base00;
            }
          )

      The 'cfg' alias can be accessed as follows:

          (
            { cfg }:
            {
              programs.«name».extension.enable = cfg.extension.enable;
            }
          )
  : `generalConfig`
    : This argument mirrors the configElements argument but intentionally lacks
      automatic safeguarding and should only be used for complex configurations
      where configElements is unsuitable.

  # Outputs

  A nix Module body, using the "plain attrs" syntax.
  This form should be used directly after your module args.
*/
{
  name,
  humanName,
  autoEnable ? true,
  extraOptions ? { },
  configElements ? [ ],
  generalConfig ? null,
}:
# Note: we only return a plain module so that calling code can use module args,
# while using this as their module body
let
  module =
    { config, lib, ... }:
    let
      cfg = config.stylix.targets.${name};

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
              }, got ${arg}")
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
    { options.stylix.targets.${name} = extraOptions; }
    module
  ];
}
