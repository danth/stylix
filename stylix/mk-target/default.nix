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
  autoEnable ? true,
  extraOptions ? { },
  configElements ? [ ],
  generalConfig ? null,
  imports ? [ ],
}:
let
  module =
    { config, lib, ... }:
    let
      cfg = config.stylix.targets.${name};
      inherit (import ./lib.nix { inherit cfg config lib; })
        mkConfig
        mkConditionalConfig
        ;
    in
    {
      inherit imports;

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
