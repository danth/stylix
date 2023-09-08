{
  pkgs,
  config,
  lib,
  ...
}: {
  options.stylix.targets.kvantum.enable =
    config.lib.stylix.mkEnableTarget "Kvantum" pkgs.stdenv.hostPlatform.isLinux;

  config = lib.mkIf config.stylix.targets.kvantum.enable (let
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
    home.packages = with pkgs; [
      qt5ct
      libsForQt5.qtstyleplugin-kvantum
      qt6Packages.qtstyleplugin-kvantum
      kvantumPackage
    ];

    qt = {
      enable = true;
      platformTheme = "qtct";
    };

    xdg.configFile."Kvantum/kvantum.kvconfig".source = (pkgs.formats.ini {}).generate "kvantum.kvconfig" {
      General.theme = "Base16Kvantum";
    };
  });
}
