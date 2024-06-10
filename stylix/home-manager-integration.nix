homeManagerModule:
{ lib, config, options, ... }:

let
  copyModules = builtins.map
    (
      { path, condition ? lib.const true }:
      { config, osConfig, ... }:
      lib.mkIf (condition config)
      (lib.setAttrByPath path (lib.mkDefault (lib.getAttrFromPath path osConfig)))
    )
    [
      { path = [ "stylix" "autoEnable" ]; }
      {
        path = [ "stylix" "base16Scheme" ];
        condition = homeConfig: config.stylix.image == homeConfig.stylix.image;
      }
      { path = [ "stylix" "cursor" "name" ]; }
      { path = [ "stylix" "cursor" "package" ]; }
      { path = [ "stylix" "cursor" "size" ]; }
      { path = [ "stylix" "enable" ]; }
      { path = [ "stylix" "fonts" "serif" ]; }
      { path = [ "stylix" "fonts" "sansSerif" ]; }
      { path = [ "stylix" "fonts" "monospace" ]; }
      { path = [ "stylix" "fonts" "emoji" ]; }
      { path = [ "stylix" "fonts" "sizes" "desktop" ]; }
      { path = [ "stylix" "fonts" "sizes" "applications" ]; }
      { path = [ "stylix" "fonts" "sizes" "terminal" ]; }
      { path = [ "stylix" "fonts" "sizes" "popups" ]; }
      { path = [ "stylix" "image" ]; }
      { path = [ "stylix" "imageScalingMode" ]; }
      { path = [ "stylix" "opacity" "desktop" ]; }
      { path = [ "stylix" "opacity" "applications" ]; }
      { path = [ "stylix" "opacity" "terminal" ]; }
      { path = [ "stylix" "opacity" "popups" ]; }
      {
        path = [ "stylix" "override" ];
        condition = homeConfig: config.stylix.base16Scheme == homeConfig.stylix.base16Scheme;
      }
      { path = [ "stylix" "polarity" ]; }
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
