# This module is a copy of the Firefox module.

{ config, lib, ... }:

let
  profileSettings = {
    settings = {
      "font.name.monospace.x-western" = config.stylix.fonts.monospace.name;
      "font.name.sans-serif.x-western" = config.stylix.fonts.sansSerif.name;
      "font.name.serif.x-western" = config.stylix.fonts.serif.name;
    };
  };
  makeProfileSettingsPair = profileName:
    lib.nameValuePair profileName profileSettings;
in {
  options.stylix.targets.librewolf = {
    enable =
      config.lib.stylix.mkEnableTarget "Librewolf" true;

    profileNames = lib.mkOption {
      description = "The Librewolf profile names to apply styling on.";
      type = lib.types.listOf lib.types.str;
      default = [ ];
    };
  };

  config = lib.mkIf (config.stylix.enable && config.stylix.targets.librewolf.enable) {
    programs.librewolf.profiles = lib.listToAttrs
      (map makeProfileSettingsPair config.stylix.targets.librewolf.profileNames);
  };
}
