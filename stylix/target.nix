{ config, lib, ... }@args:

with lib;

{
  options.stylix.autoEnable =
    mkEnableOption
    "styling installed targets"
    // {
      default = true;
      example = false;
    };

  config.lib.stylix.mkEnableTarget =
    humanName:

    # If the module only touches options under its target (programs.target.*)
    # then this can simply be `true`, as those options are already gated by the
    # upstream enable option.
    #
    # Otherwise, use `config` to check whether the target is enabled.
    #
    # If some manual setup is required, or the module leads to the target
    # being installed if it wasn't already, set this to `false`.
    autoEnable:
      mkEnableOption
      "styling for ${humanName}"
      // {
        default = config.stylix.autoEnable && autoEnable;
      }
      // optionalAttrs autoEnable {
        defaultText = literalExpression "stylix.autoEnable";
      };
}
