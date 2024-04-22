{ config, lib, ... }@args:

with lib;

let
  fromOs = import ./fromos.nix { inherit lib args; };
in {
  options.stylix.autoEnable = mkOption {
    description = "Whether to automatically enable styling for installed targets.";
    type = types.bool;
    default = fromOs [ "autoEnable" ] true;
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

      # We can't substitute the target name into this description because some
      # don't make sense: "if the desktop background using Feh is installed"
      defaultText = literalMD ''
        `true` if `stylix.autoEnable == true` and the target is installed,
        otherwise `false`.
      '';

      default = config.stylix.autoEnable && autoEnable;
    };
}
