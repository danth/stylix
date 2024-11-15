homeManagerModule:
{
  lib,
  config,
  options,
  ...
}:

let
  copyModules =
    builtins.map
      (
        {
          path,
          condition ? lib.const true,
        }:
        { config, osConfig, ... }:
        lib.mkIf (condition config) (
          lib.setAttrByPath path (lib.mkDefault (lib.getAttrFromPath path osConfig))
        )
      )
      [
        {
          path = [
            "stylix"
            "autoEnable"
          ];
        }
        {
          path = [
            "stylix"
            "base16Scheme"
          ];
          condition = homeConfig: config.stylix.image == homeConfig.stylix.image;
        }
        {
          path = [
            "stylix"
            "cursor"
            "name"
          ];
        }
        {
          path = [
            "stylix"
            "cursor"
            "package"
          ];
        }
        {
          path = [
            "stylix"
            "cursor"
            "size"
          ];
        }
        {
          path = [
            "stylix"
            "enable"
          ];
        }
        {
          path = [
            "stylix"
            "fonts"
            "serif"
          ];
        }
        {
          path = [
            "stylix"
            "fonts"
            "sansSerif"
          ];
        }
        {
          path = [
            "stylix"
            "fonts"
            "monospace"
          ];
        }
        {
          path = [
            "stylix"
            "fonts"
            "emoji"
          ];
        }
        {
          path = [
            "stylix"
            "fonts"
            "sizes"
            "desktop"
          ];
        }
        {
          path = [
            "stylix"
            "fonts"
            "sizes"
            "applications"
          ];
        }
        {
          path = [
            "stylix"
            "fonts"
            "sizes"
            "terminal"
          ];
        }
        {
          path = [
            "stylix"
            "fonts"
            "sizes"
            "popups"
          ];
        }
        {
          path = [
            "stylix"
            "image"
          ];
        }
        {
          path = [
            "stylix"
            "imageScalingMode"
          ];
        }
        {
          path = [
            "stylix"
            "opacity"
            "desktop"
          ];
        }
        {
          path = [
            "stylix"
            "opacity"
            "applications"
          ];
        }
        {
          path = [
            "stylix"
            "opacity"
            "terminal"
          ];
        }
        {
          path = [
            "stylix"
            "opacity"
            "popups"
          ];
        }
        {
          path = [
            "stylix"
            "override"
          ];
          condition =
            homeConfig: config.stylix.base16Scheme == homeConfig.stylix.base16Scheme;
        }
        {
          path = [
            "stylix"
            "polarity"
          ];
        }
      ];

in
{
  options.stylix.homeManagerIntegration = {
    followSystem = lib.mkOption {
      description = ''
        When this option is `true`, Home Manager configurations will follow
        the NixOS configuration by default, rather than using the standard
        default settings.

        This only applies to Home Manager configurations managed by
        [`stylix.homeManagerIntegration.autoImport`](#stylixhomemanagerintegrationautoimport).
      '';
      type = lib.types.bool;
      default = true;
      example = false;
    };

    autoImport = lib.mkOption {
      description = ''
        Whether to import Stylix automatically for every Home Manager user.

        This only works if you are using `home-manager.users.«name»` within
        your NixOS configuration, rather than running Home Manager independently.
      '';
      type = lib.types.bool;
      default = true;
      example = false;
    };
  };

  config = lib.optionalAttrs (options ? home-manager) (
    lib.mkIf config.stylix.homeManagerIntegration.autoImport {
      home-manager.sharedModules =
        [ homeManagerModule ]
        ++ (lib.optionals config.stylix.homeManagerIntegration.followSystem copyModules);
    }
  );
}
