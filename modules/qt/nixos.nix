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
        Platform for QT.

        Defaults to the standard platform of the configured DE.

        Fallback to qtct.
      '';
      type = lib.types.str;
      default = "qtct";
    };
  };

  config = lib.mkIf (config.stylix.enable && config.stylix.targets.qt.enable) {

    stylix.targets.qt.platform =
      if config.services.xserver.desktopManager.gnome.enable then
        lib.mkDefault "gnome"
      else if config.services.xserver.desktopManager.plasma5.enable then
        lib.mkDefault "kde"
      else if config.services.xserver.desktopManager.lxqt.enable then
        lib.mkDefault "lxqt"
      else
        lib.mkDefault "qt5ct";
    qt = {
      enable = true;
      style = lib.mkIf (
        recommendedStyle ? "${config.qt.platformTheme}"
      ) recommendedStyle."${config.qt.platformTheme}";
      platformTheme =
        if config.stylix.targets.qt.platform == "qtct" then
          "qt5ct"
        else
          config.stylix.targets.qt.platform;
    };

  };
}
