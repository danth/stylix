{ pkgs, config, lib, ... }:

let
  theme = config.lib.stylix.colors {
    templateRepo = pkgs.fetchFromGitHub {
      owner = "tinted-theming";
      repo = "base16-helix";
      rev = "6bc29bacf5d7d2d5865f0935c8af20cec81ce91f";
      sha256 = "pVDxOgk+WV+xEwAeZCCjNTguQLo/sw7VUHeuaHX6uNM=";
    };
  };

in {
  options.stylix.targets.helix.enable =
    config.lib.stylix.mkEnableTarget "Helix" config.programs.helix.enable;

  config = lib.mkIf config.stylix.targets.helix.enable {
    programs.helix.settings.theme = "stylix";
    xdg.configFile."helix/themes/stylix.toml".source = theme;
  };
}
