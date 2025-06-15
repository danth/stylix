{
  lib,
  pkgs,
  config,
  ...
}:

let

  recommendedStyle = {
    gnome = if config.stylix.polarity == "dark" then "adwaita-dark" else "adwaita";
    kde = "breeze";
    qtct = "kvantum";
  };

in
{
  options.stylix.targets.qt = {
    enable = config.lib.stylix.mkEnableTarget "QT" pkgs.stdenv.hostPlatform.isLinux;
    platform = lib.mkOption {
      description = ''
        Selects the platform theme to use for Qt applications.

        Defaults to the standard platform used in the configured DE.
      '';
      type = with lib.types; nullOr str;
      default = null;
    };
  };

  config =
    let
      inherit (config.services.xserver.desktopManager) gnome plasma5 lxqt;
      inherit (config.services.desktopManager) plasma6;
    in
    lib.mkIf (config.stylix.enable && config.stylix.targets.qt.enable) {
      stylix.targets.qt.platform =
        if gnome.enable && !(plasma5.enable || plasma6.enable || lxqt.enable) then
          "gnome"
        else if plasma5.enable && !(gnome.enable || plasma6.enable || lxqt.enable) then
          "kde"
        else if plasma6.enable && !(gnome.enable || plasma5.enable || lxqt.enable) then
          "kde6"
        else if lxqt.enable && !(gnome.enable || plasma5.enable || plasma6.enable) then
          "lxqt"
        else if !plasma6.enable then
          "qtct"
        else
          null;
      qt = {
        enable = true;
        style = recommendedStyle."${config.qt.platformTheme}" or null;
        platformTheme =
          if config.stylix.targets.qt.platform == "qtct" then
            "qt5ct"
          else
            config.stylix.targets.qt.platform;
      };
    };
}
