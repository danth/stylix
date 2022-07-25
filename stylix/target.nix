{ config, lib, ... }@args:

with lib;

{
  options.stylix.autoEnable = mkOption {
    description = "Whether to automatically enable styling for installed targets.";
    type = types.bool;
    default = true;
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

    mkOption {
      description = "Whether to style ${humanName}.";
      type = types.bool;
      defaultText = ''
        When <literal>stylix.autoEnable</literal> is <literal>true</literal>:
        Enabled when ${humanName} is installed.

        When <literal>stylix.autoEnable</literal> is <literal>false</literal>:
        Defaults to <literal>false</literal>.
      '';
      default = config.stylix.autoEnable && autoEnable;
    };
}
