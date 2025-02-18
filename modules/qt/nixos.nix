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

  config =
    let
      broken = config.services.desktopManager.plasma6.enable;
      warning = {
        warnings = [
          "stylix: qt: Plasma6 is currently unsupported: https://github.com/nix-community/home-manager/issues/5098"
        ];
      };
      default = lib.mkIf (config.stylix.enable && config.stylix.targets.qt.enable) {

        stylix.targets.qt.platform =
          with config.services.xserver.desktopManager;
          if gnome.enable && !(plasma5.enable || lxqt.enable) then
            "gnome"
          else if plasma5.enable && !(gnome.enable || lxqt.enable) then
            "kde"
          else if lxqt.enable && !(gnome.enable || plasma5.enable) then
            "lxqt"
          else
            "qtct";
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
    in
    lib.mkMerge [
      (lib.mkIf broken warning)
      (lib.mkIf (!broken) default)
    ];
}
