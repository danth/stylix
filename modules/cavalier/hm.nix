{ config, lib, ... }:

{
  options.stylix.targets.cavalier = {
    enable = config.lib.stylix.mkEnableTarget "Cavalier" true;
  };

  config =
    let
      cfg = config.stylix.targets.cavalier;
    in
    lib.mkIf (config.stylix.enable && cfg.enable) {
      programs.cavalier.settings.general = {
        ColorProfiles =
          with config.lib.stylix.colors;
          lib.singleton {
            Name = "Stylix";
            FgColors = lib.singleton base05;
            BgColors = lib.singleton base00;
          };
        ActiveProfile = 0;
      };
    };
}
