homeManagerModule:
{ lib, config, options, ... }:

let
  alwaysCopy = path: {
    inherit path;
    condition = lib.const true;
  };

  copyModules = builtins.map
    (
      { path, condition }:
      { config, osConfig, ... }:
      lib.mkIf (condition config)
      (lib.setAttrByPath path (lib.mkDefault (lib.getAttrFromPath path osConfig)))
    )
    [
      (alwaysCopy [ "stylix" "autoEnable" ])
      {
        path = [ "stylix" "base16Scheme" ];
        condition = homeConfig: config.stylix.image == homeConfig.stylix.image;
      }
      (alwaysCopy [ "stylix" "cursor" "name" ])
      (alwaysCopy [ "stylix" "cursor" "package" ])
      (alwaysCopy [ "stylix" "cursor" "size" ])
      (alwaysCopy [ "stylix" "fonts" "serif" ])
      (alwaysCopy [ "stylix" "fonts" "sansSerif" ])
      (alwaysCopy [ "stylix" "fonts" "monospace" ])
      (alwaysCopy [ "stylix" "fonts" "emoji" ])
      (alwaysCopy [ "stylix" "fonts" "sizes" "desktop" ])
      (alwaysCopy [ "stylix" "fonts" "sizes" "applications" ])
      (alwaysCopy [ "stylix" "fonts" "sizes" "terminal" ])
      (alwaysCopy [ "stylix" "fonts" "sizes" "popups" ])
      (alwaysCopy [ "stylix" "image" ])
      (alwaysCopy [ "stylix" "opacity" "desktop" ])
      (alwaysCopy [ "stylix" "opacity" "applications" ])
      (alwaysCopy [ "stylix" "opacity" "terminal" ])
      (alwaysCopy [ "stylix" "opacity" "popups" ])
      {
        path = [ "stylix" "override" ];
        condition = homeConfig: config.stylix.base16Scheme == homeConfig.stylix.base16Scheme;
      }
      (alwaysCopy [ "stylix" "polarity" ])
    ];

in {
  options.stylix.homeManagerIntegration = {
    followSystem = lib.mkOption {
      description = ''
        When this option is `true`, Home Manager will follow
        the system theme by default, rather than requiring a theme to be set.

        This will only affect Home Manager configurations which are built
        within the NixOS configuration.
      '';
      type = lib.types.bool;
      default = true;
    };

    autoImport = lib.mkOption {
      description = ''
        Whether to enable Stylix automatically for every user.

        This only applies to users for which Home Manager is set up within the
        NixOS configuration.
      '';
      type = lib.types.bool;
      default = options ? home-manager;
      defaultText = lib.literalMD ''
        `true` when Home Manager is present.
      '';
    };
  };

  config =
    lib.optionalAttrs (options ? home-manager)
    (lib.mkIf config.stylix.homeManagerIntegration.autoImport {
      home-manager.sharedModules = [ homeManagerModule ] ++ copyModules;
    });
}
