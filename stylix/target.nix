{ config, lib, ... }@args:

with lib;

{
  options.stylix = {
    autoEnable = mkEnableOption "styling installed targets" // {
      default = import ./fromos.nix { inherit lib args; } [ "autoEnable" ] true;
      example = false;
    };

    enable = mkEnableOption "Stylix";
  };

  config.lib.stylix.mkEnableTarget = let
    cfg = config.stylix;
  in
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
        default = cfg.enable && cfg.autoEnable && autoEnable;
      }
      // optionalAttrs autoEnable {
        defaultText = literalExpression "stylix.enable && stylix.autoEnable";
      };
}
