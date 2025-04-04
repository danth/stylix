{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.stylix.targets.steam;
in
{
  options.stylix.targets.steam = {
    enable = config.lib.stylix.mkEnableTarget "Steam" true;
    adwaitaForSteam.enable = config.lib.stylix.mkEnableTarget "Adwaita for Steam" true;
  };

  config =
    lib.mkIf (config.stylix.enable && cfg.enable && cfg.adwaitaForSteam.enable)
      {
        programs.steam.package = pkgs.steam.override {
          extraArgs = "-noverifyfiles";
        };
      };
}
