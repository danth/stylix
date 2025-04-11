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
    in
    {
      mkEnableTarget =
        humanName: autoEnable:
        lib.mkEnableOption "theming for ${humanName}"
        // {
          default = cfg.enable && cfg.autoEnable && autoEnable;
          example = !autoEnable;
        }
        // lib.optionalAttrs autoEnable {
          defaultText = lib.literalMD "same as `stylix.autoEnable`";
        };
      mkEnableWallpaper =
        humanName: autoEnable:
        lib.mkOption {
          default = config.stylix.image != null && autoEnable;
          example = config.stylix.image == null;
          description = "Whether to set the wallpaper for ${humanName}.";
          type = lib.types.bool;
        }
        // lib.optionalAttrs autoEnable {
          defaultText = lib.literalMD "`stylix.image != null`";
        };

      mkTarget =
        {
          name,
          humanName,
          autoEnable ? true,
          extraOptions ? { },
          configElements ? [ ],
          generalConfig ? null,
        }:
        let
          cfg = config.stylix.targets.${name};
        in
        {
          options.stylix.targets.${name} = {
            enable = config.lib.stylix.mkEnableTarget humanName autoEnable;
          } // extraOptions;

          config =
            let
              provideStylixArgs =
                args:
                (lib.mkMerge map (
                  arg:
                  if arg == "cfg" then
                    {
                      inherit cfg;
                    }
                  else
                    {
                      ${arg} = config.stylix.${arg};
                    }
                ) args);
              provideAvailableStylixArgs =
                args:
                (lib.mkMerge map (
                  arg:
                  if arg == "cfg" then
                    {
                      inherit cfg;
                    }
                  else if (config.stylix ? arg) then
                    {
                      ${arg} = config.stylix.${arg};
                    }
                  else
                    { }
                ) args);
              mkConfig =
                fn:
                let
                  args = cfg builtins.attrNames (lib.functionArgs fn);
                in
                fn (provideAvailableStylixArgs args);
              mkConditionalConfig =
                fn:
                let
                  args = cfg builtins.attrNames (lib.functionArgs fn);
                in
                fn:
                if
                  builtins.all (arg: (arg == "cfg") || (builtins.hasAttr config.stylix arg)) args
                then
                  fn (provideStylixArgs args)
                else
                  { };
            in
            lib.mkIf (config.stylix.enable && cfg.enable) (
              lib.mkMerge (
                (map mkConditionalConfig configElements) ++ [ (mkConfig generalConfig) ]
              )
            );
        };
    };
}
