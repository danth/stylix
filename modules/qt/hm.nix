{
  pkgs,
  config,
  lib,
  ...
}: {
  options.stylix.targets.qt = {
    enable = config.lib.stylix.mkEnableTarget "QT" pkgs.stdenv.hostPlatform.isLinux;
    enableDefaultIcons = lib.mkOption {
      description = "Default QT Icons";
      type = lib.types.bool;
      default = true;
    };
  };

  config = lib.mkIf config.stylix.targets.qt.enable (let
    cfg = config.stylix.targets.qt;
    kvconfig = config.lib.stylix.colors {
      template = ./kvconfig.mustache;
      extension = ".kvconfig";
    };
    svg = config.lib.stylix.colors {
      template = ./kvantum-svg.mustache;
      extension = "svg";
    };
    kvantumPackage = pkgs.runCommandLocal "base16-kvantum" {} ''
      mkdir -p $out/share/Kvantum/Base16Kvantum
      cat ${kvconfig} >>$out/share/Kvantum/Base16Kvantum/Base16Kvantum.kvconfig
      cat ${svg} >>$out/share/Kvantum/Base16Kvantum/Base16Kvantum.svg
    '';
  in {
    home.packages = with pkgs;
      [
        qt5ct
        libsForQt5.qtstyleplugin-kvantum
        qt6Packages.qtstyleplugin-kvantum
        kvantumPackage
      ]
      ++ (lib.optionals cfg.enableDefaultIcons [
        papirus-icon-theme
      ]);

    qt = {
      enable = true;
      platformTheme = "qtct";
    };

    xdg.configFile."Kvantum/kvantum.kvconfig".source = (pkgs.formats.ini {}).generate "kvantum.kvconfig" {
      General.theme = "Base16Kvantum";
    };

    xdg.configFile."Kvantum/Base16Kvantum".source = "${kvantumPackage}/share/Kvantum/Base16Kvantum";

    xdg.configFile."qt5ct/qt5ct.conf".text =
      ''
        [Appearance]
        style=kvantum
      ''
      + (lib.optionalString cfg.enableDefaultIcons ''
        icon_theme=ePapirus-Dark
      '');

    xdg.configFile."qt6ct/qt6ct.conf".text =
      ''
        [Appearance]
        style=kvantum
      ''
      + (lib.optionalString cfg.enableDefaultIcons ''
        icon_theme=ePapirus-Dark
      '');
  });
}
