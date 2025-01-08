{ config, lib, ... }:

{
  options.stylix.targets.cavalier.enable =
    config.lib.stylix.mkEnableTarget "Cavalier" true;

  config =
    let
      cfg = config.stylix.targets.cavalier;
    in
    lib.mkIf (config.stylix.enable && cfg.enable) {
      programs.cavalier.settings.general = {
        ColorProfiles =
          let
            inherit (config.lib.stylix) colors;
          in
          lib.singleton {
            Name = "Stylix";
            FgColors = [ colors.base05 ];
            BgColors = [ colors.base00 ];
          };
        ActiveProfile = 0;
      };
    };
}
