{
  lib,
  config,
  options,
  pkgs,
  ...
}:

let
  copyModules =
    map
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
        # keep-sorted start block=yes
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
          ];
          condition = _homeConfig: !pkgs.stdenv.hostPlatform.isDarwin;
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
            "emoji"
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
            "sansSerif"
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
            "sizes"
            "applications"
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
            "popups"
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
            "applications"
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
            "popups"
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
        {
          path = [
            "stylix"
            "targets"
            "qt"
            "platform"
          ];
        }
        # keep-sorted end
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
      default = config.stylix.enable;
      defaultText = "config.stylix.enable";
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

    module = lib.mkOption {
      description = ''
        The Home Manager module to be imported.
      '';
      internal = true;
      readOnly = true;
    };
  };

  config = lib.optionalAttrs (options ? home-manager) (
    lib.mkMerge [
      (lib.mkIf config.stylix.homeManagerIntegration.autoImport {
        home-manager.sharedModules =
          [
            config.stylix.homeManagerIntegration.module
          ]
          ++ (lib.optionals config.stylix.homeManagerIntegration.followSystem copyModules);
      })
      (lib.mkIf config.home-manager.useGlobalPkgs {
        home-manager.sharedModules = lib.singleton {
          config.stylix.overlays.enable = false;
        };
      })
    ]
  );
}
