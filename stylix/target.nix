{ config, lib, ... }:

{
  options.stylix = {
    enable = lib.mkOption {
      description = ''
        Whether to enable Stylix.

        When this is `false`, all theming is disabled and all other options
        are ignored.
      '';
      type = lib.types.bool;
      default = false;
      example = true;
    };

    autoEnable = lib.mkOption {
      description = ''
        Whether to enable targets by default.

        When this is `false`, all targets are disabled unless explicitly enabled.

        When this is `true`, most targets are enabled by default. A small number
        remain off by default, because they require further manual setup, or
        they are only applicable in specific circumstances which cannot be
        detected automatically.
      '';
      type = lib.types.bool;
      default = true;
      example = false;
    };
  };

  config.lib.stylix.mkEnableTarget =
    let
      cfg = config.stylix;
    in
    humanName: autoEnable:
    lib.mkEnableOption "theming for ${humanName}"
    // {
      default = cfg.enable && cfg.autoEnable && autoEnable;
      example = !autoEnable;
    }
    // lib.optionalAttrs autoEnable {
      defaultText = lib.literalMD "same as [`stylix.autoEnable`](#stylixautoenable)";
    };
}
