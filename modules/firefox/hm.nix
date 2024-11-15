# Consider also updating the LibreWolf module when updating this module,
# as they are very similar.

{ config, lib, ... }:

let
  profileSettings = {
    settings = {
      "font.name.monospace.x-western" = config.stylix.fonts.monospace.name;
      "font.name.sans-serif.x-western" = config.stylix.fonts.sansSerif.name;
      "font.name.serif.x-western" = config.stylix.fonts.serif.name;
    };
  };
  makeProfileSettingsPair =
    profileName: lib.nameValuePair profileName profileSettings;
  derivatives = [
    {
      path = "firefox";
      name = "Firefox";
    }
    {
      path = "librewolf";
      name = "LibreWolf";
    }
  ];
in
{
  options.stylix.targets = lib.listToAttrs (
    map (
      drv:
      lib.nameValuePair drv.path {
        enable = config.lib.stylix.mkEnableTarget drv.name true;

        profileNames = lib.mkOption {
          description = "The ${drv.name} profile names to apply styling on.";
          type = lib.types.listOf lib.types.str;
          default = [ ];
        };
      }
    ) derivatives
  );

  config = lib.mkMerge (
    map (
      drv:
      lib.mkIf (config.stylix.enable && config.stylix.targets.${drv.path}.enable) {
        programs.${drv.path}.profiles = lib.listToAttrs (
          map makeProfileSettingsPair config.stylix.targets.${drv.path}.profileNames
        );
      }
    ) derivatives
  );
}
