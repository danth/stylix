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
      type = lib.types.str;
    };
  };

  config = lib.mkIf (config.stylix.enable && config.stylix.targets.qt.enable) {
    stylix.targets.qt.platform =
      with config.services.xserver.desktopManager;
      if gnome.enable && !(plasma5.enable || lxqt.enable) then
        lib.mkDefault "gnome"
      else if plasma5.enable && !(gnome.enable || lxqt.enable) then
        lib.mkDefault "kde"
      else if lxqt.enable && !(gnome.enable || plasma5.enable) then
        lib.mkDefault "lxqt"
      else
        lib.mkDefault "qtct";
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
