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
  options.stylix.targets.firefox = {
    enable =
      config.lib.stylix.mkEnableTarget "Firefox" true;

    profileNames = lib.mkOption {
      description = "The Firefox profile names to apply styling on.";
      type = lib.types.listOf lib.types.str;
      default = [ ];
    };
  };

  config = lib.mkIf (config.stylix.enable && config.stylix.targets.firefox.enable) {
    programs.firefox.profiles = lib.listToAttrs
      (map makeProfileSettingsPair config.stylix.targets.firefox.profileNames);
  };
}
