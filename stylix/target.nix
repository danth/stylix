{ config, lib, ... }@args:

with lib;

{
  options.stylix = {
    enable = mkOption {
      description = ''
        Whether to enable Stylix.

        When this is `false`, all theming is disabled and all other options
        are ignored.
      '';
      type = types.bool;
      default = import ./fromos.nix { inherit lib args; } [ "enable" ] false;
      example = true;
    };

    autoEnable = mkOption {
      description = ''
        Whether to enable targets by default.

        When this is `false`, all targets are disabled unless explicitly enabled.

        When this is `true`, most targets are enabled by default. A small number
        remain off by default, because they require further manual setup, or
        they are only applicable in specific circumstances which cannot be
        detected automatically.
      '';
      type = types.bool;
      default = import ./fromos.nix { inherit lib args; } [ "autoEnable" ] true;
      example = false;
    };
  };

  config.lib.stylix.mkEnableTarget = let
    cfg = config.stylix;
  in
    humanName:
    autoEnable:
      mkEnableOption
      "theming for ${humanName}"
      // {
        default = cfg.autoEnable && autoEnable;
        example = !autoEnable;
      }
      // optionalAttrs autoEnable {
        defaultText = literalMD "same as [`stylix.autoEnable`](#stylixautoenable)";
      };
}
