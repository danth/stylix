{
  pkgs,
  config,
  lib,
  ...
}:
{
  options.stylix.targets.qt = {
    enable = config.lib.stylix.mkEnableTarget "QT" pkgs.stdenv.hostPlatform.isLinux;
    platform = lib.mkOption {
      description = ''
        Platform for QT.

        Defaults to the standard platform of the configured DE in NixOS when
        `stylix.homeManagerIntegration.followSystem = true`.

        Fallback to qtct.
      '';
      type = lib.types.str;
      default = "qtct";
    };
  };

  config = lib.mkIf config.stylix.targets.qt.enable (
    let
      cfg = config.stylix;
      iconTheme =
        if (cfg.polarity == "dark") then cfg.iconTheme.dark else cfg.iconTheme.light;

      recommendedStyle = {
        gnome = if config.stylix.polarity == "dark" then "adwaita-dark" else "adwaita";
        kde = "breeze";
        qtct = "kvantum";
      };

      kvantumPackage =
        let
          kvconfig = config.lib.stylix.colors {
            template = ./kvconfig.mustache;
            extension = ".kvconfig";
          };
          svg = config.lib.stylix.colors {
            template = ./kvantum-svg.mustache;
            extension = "svg";
          };
        in
        pkgs.runCommandLocal "base16-kvantum" { } ''
          directory="$out/share/Kvantum/Base16Kvantum"
          mkdir --parents "$directory"
          ln -s ${kvconfig} "$directory/Base16Kvantum.kvconfig"
          ln -s ${svg} "$directory/Base16Kvantum.svg"
        '';
    in
    {
      warnings = lib.optional (cfg.targets.qt.platform != "qtct") ''
        Stylix has not yet implemented qt styling for any platforms other than "qtct".
        We are working on it.
      '';

      home.packages = lib.optional (config.qt.style.name == "kvantum") kvantumPackage;

      qt = {
        enable = true;
        style.name = lib.mkIf (
          recommendedStyle ? "${config.qt.platformTheme.name}"
        ) recommendedStyle."${config.qt.platformTheme.name}";
        platformTheme.name = cfg.targets.qt.platform;
      };

      xdg.configFile = lib.mkMerge [
        (lib.mkIf (config.qt.style.name == "kvantum") {
          "Kvantum/kvantum.kvconfig".source =
            (pkgs.formats.ini { }).generate "kvantum.kvconfig"
              {
                General.theme = "Base16Kvantum";
              };

          "Kvantum/Base16Kvantum".source =
            "${kvantumPackage}/share/Kvantum/Base16Kvantum";
        })

        (lib.mkIf (config.qt.platformTheme.name == "qtct") {
          "qt5ct/qt5ct.conf".text = ''
            [Appearance]
            style=${config.qt.style.name}
            icon_theme=${iconTheme}
          '';
          "qt6ct/qt6ct.conf".text = ''
            [Appearance]
            style=${config.qt.style.name}
            icon_theme=${iconTheme}
          '';
        })
      ];
    }
  );
}
