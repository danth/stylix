/**
  Provides a consistent target interface, minimizing boilerplate and
  automatically safeguarding declarations related to disabled options.

  # Type

  ```
  mkTarget :: AttrSet -> ModuleBody
  ```

  Where `ModuleBody` is a module that doesn't take any arguments. This allows
  the caller to use module arguments.

  # Examples

  The `modules/«MODULE»/«PLATFORM».nix` modules should use this function as
  follows:

  ```nix
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
  ```

  # Inputs

  `config` (Attribute set)

  : `name` (String)
    : The target name used to generate options in the `stylix.targets.${name}`
      namespace.

    `humanName` (String)
    : The descriptive target name passed to the lib.mkEnableOption function
      when generating the `stylix.targets.${name}.enable` option.

    `autoEnable` (Boolean)
    : Whether the target should be automatically enabled by default according
      to the `stylix.autoEnable` option.

      This should be disabled if manual setup is required or if auto-enabling
      causes issues.

      The default (`true`) is inherited from `mkEnableTargetWith`.

    `autoEnableExpr` (String)
    : A string representation of `autoEnable`, for use in documentation.

      Not required if `autoEnable` is a literal `true` or `false`, but **must**
      be used when `autoEnable` is a dynamic expression.

      E.g. `"pkgs.stdenv.hostPlatform.isLinux"`.

    `autoWrapEnableExpr` (Boolean)
    : Whether to automatically wrap `autoEnableExpr` with parenthesis, when it
      contains a potentially problematic infix.

      The default (`true`) is inherited from `mkEnableTargetWith`.

    `enableExample` (Boolean or literal expression)
    : An example to include on the enable option. The default is calculated
      automatically by `mkEnableTargetWith` and depends on `autoEnable` and
      whether an `autoEnableExpr` is used.

    `extraOptions` (Attribute set)
    : Additional options to be added in the `stylix.targets.${name}` namespace
      along the `stylix.targets.${name}.enable` option.

      For example, an extension guard used in the configuration can be declared
      as follows:
      ```nix
      { extension.enable = lib.mkEnableOption "the bloated dependency"; }
      ```

    `configElements` (List or attribute set or function)
    : Configuration functions that are automatically safeguarded when any of
      their arguments is disabled. The provided `cfg` argument conveniently
      aliases to `config.stylix.targets.${name}`.

      For example, the following configuration is not merged if the stylix
      colors option is null:

      ```nix
      (
        { colors }:
        {
          programs.«name».theme.background = colors.base00;
        }
      )
      ```

      The `cfg` alias can be accessed as follows:

      ```nix
      (
        { cfg }:
        {
          programs.«name».extension.enable = cfg.extension.enable;
        }
      )
      ```

    `generalConfig` (Attribute set or function)
    : This argument mirrors the `configElements` argument but intentionally
      lacks automatic safeguarding and should only be used for complex
      configurations where `configElements` is unsuitable.

  # Environment

  The function is provided alongside module arguments in any modules imported
  through `/stylix/autoload.nix`.
*/

# TODO: Ideally, in the future, this function returns an actual module by better
# integrating with the /stylix/autoload.nix logic, allowing the following target
# simplification and preventing access to unguarded module arguments by
# requiring /modules/<MODULE>/<PLATFORM>.nix files to be attribute sets instead
# of modules:
#
#     {
#       name = "example";
#       humanName = "Example Target";
#
#       generalConfig =
#         { lib, pkgs }:
#         lib.mkIf complexCondition {
#           home.packages = [ pkgs.hello ];
#         };
#
#       configElements = [
#         { programs.example.theme.name = "stylix"; }
#
#         (
#           { colors }:
#           {
#             programs.example.theme.background = colors.base00;
#           }
#         )
#
#         (
#           { fonts }:
#           {
#             programs.example.font.name = fonts.monospace.name;
#           }
#         )
#       ];
#     }
{
  name,
  humanName,
  autoEnable ? null,
  autoEnableExpr ? null,
  autoWrapEnableExpr ? null,
  enableExample ? null,
  extraOptions ? { },
  configElements ? [ ],
  generalConfig ? null,
  imports ? [ ],
}@args:
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
    in
    {
      inherit imports;

      options.stylix.targets.${name}.enable =
        let
          enableArgs =
            {
              name = humanName;
            }
            // lib.optionalAttrs (args ? autoEnable) { inherit autoEnable; }
            // lib.optionalAttrs (args ? autoEnableExpr) { inherit autoEnableExpr; }
            // lib.optionalAttrs (args ? autoWrapEnableExpr) {
              autoWrapExpr = autoWrapEnableExpr;
            }
            // lib.optionalAttrs (args ? enableExample) { example = enableExample; };
        in
        config.lib.stylix.mkEnableTargetWith enableArgs;

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
