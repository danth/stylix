{ config, lib, ... }:

let
  profileSettings = {
    settings = let
      inherit (config.stylix) fonts;
    in {
      "font.name.monospace.x-western" = (builtins.head fonts.monospace).name;
      "font.name.sans-serif.x-western" = (builtins.head fonts.sansSerif).name;
      "font.name.serif.x-western" = (builtins.head fonts.serif).name;
    };
  };
  makeProfileSettingsPair = profileName:
    lib.nameValuePair profileName profileSettings;
in {
  options.stylix.targets.firefox = {
    enable =
      config.lib.stylix.mkEnableTarget "Firefox" config.programs.firefox.enable;

    profileNames = lib.mkOption {
      description = "The Firefox profile names to apply styling on.";
      type = lib.types.listOf lib.types.str;
      default = [ ];
    };
  };

  config = lib.mkIf config.stylix.targets.firefox.enable {
    programs.firefox.profiles = lib.listToAttrs
      (map makeProfileSettingsPair config.stylix.targets.firefox.profileNames);
  };
}
