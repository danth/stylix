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

  config.lib.stylix =
    let
      cfg = config.stylix;
      self = config.lib.stylix;
    in
    {
      mkEnableTarget =
        humanName: autoEnable:
        self.mkEnableIf {
          description = "Whether to enable theming for ${humanName}.";
          default = cfg.autoEnable && autoEnable;
          ${if autoEnable then "defaultText" else null} =
            lib.literalExpression "stylix.autoEnable";
          example = !autoEnable;
        };

      mkEnableWallpaper =
        humanName: autoEnable:
        self.mkEnableIf {
          description = "Whether to set the wallpaper for ${humanName}.";
          default = config.stylix.image != null && autoEnable;
          defaultText =
            if autoEnable then lib.literalExpression "stylix.image != null" else false;
          example = config.stylix.image == null;
        };

      mkEnableIf =
        {
          description,
          default,
          defaultText ? null,
          example ? if args ? defaultText then true else !default,
        }@args:
        lib.mkOption {
          type = lib.types.bool;
          defaultText = if args ? defaultText then defaultText else default;
          inherit
            default
            description
            example
            ;
        };
    };
}
