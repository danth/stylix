{ config, lib, ... }@args:

with lib;

{
  options.stylix.autoEnable = let
    fromOs = import ./fromos.nix { inherit lib args; } [ "autoEnable" ];
  in
    mkEnableOption
    "styling installed targets"
    // {
      default = fromOs true;
      example = fromOs false;
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

        # We can't substitute the target name into this description because some
        # don't make sense: "if the desktop background using Feh is installed"
        defaultText = literalMD ''
          `true` if `stylix.autoEnable == true` and the target is installed,
          otherwise `false`.
        '';
      };
}
