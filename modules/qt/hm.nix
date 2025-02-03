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
        Selects the platform theme to use for Qt applications.

        Defaults to the standard platform theme used in the configured DE in NixOS when
        `stylix.homeManagerIntegration.followSystem = true`.
      '';
      type = lib.types.str;
      default = "qtct";
    };
  };

  config = lib.mkIf config.stylix.targets.qt.enable (
    let
      iconTheme =
        if (config.stylix.polarity == "dark") then
          config.stylix.iconTheme.dark
        else
          config.stylix.iconTheme.light;

      recommendedStyles = {
        gnome = if config.stylix.polarity == "dark" then "adwaita-dark" else "adwaita";
        kde = "breeze";
        qtct = "kvantum";
      };

      recommendedStyle = recommendedStyles."${config.qt.platformTheme.name}" or null;

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
          cp ${kvconfig} "$directory/Base16Kvantum.kvconfig"
          cp ${svg} "$directory/Base16Kvantum.svg"
        '';
    in
    {
      warnings =
        (lib.optional (config.stylix.targets.qt.platform != "qtct")
          "stylix: qt: `config.stylix.targets.qt.platform` other than 'qtct' are currently unsupported: ${config.stylix.targets.qt.platform}. Support may be added in the future."
        )
        ++ (lib.optional (config.qt.style.name != recommendedStyle)
          "stylix: qt: Changing `config.qt.style` is unsupported and may result in breakage! Use with caution!"
        );

      home.packages = lib.optional (config.qt.style.name == "kvantum") kvantumPackage;

      qt = {
        enable = true;
        style.name = recommendedStyle;
        platformTheme.name = config.stylix.targets.qt.platform;
      };

      xdg.configFile =
        let
          qtctConf =
            ''
              [Appearance]
            ''
            + lib.optionalString (config.qt.style ? name) ''
              style=${config.qt.style.name}
            ''
            + lib.optionalString (iconTheme != null) ''
              icon_theme=${iconTheme}
            '';

        in
        lib.mkMerge [
          (lib.mkIf (config.qt.style.name == "kvantum") {
            "Kvantum/kvantum.kvconfig".source =
              (pkgs.formats.ini { }).generate "kvantum.kvconfig"
                { General.theme = "Base16Kvantum"; };

            "Kvantum/Base16Kvantum".source =
              "${kvantumPackage}/share/Kvantum/Base16Kvantum";
          })

          (lib.mkIf (config.qt.platformTheme.name == "qtct") {
            "qt5ct/qt5ct.conf".text = qtctConf;
            "qt6ct/qt6ct.conf".text = qtctConf;
          })
        ];
    }
  );
}
