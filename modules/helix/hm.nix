{
  config,
  lib,
  ...
}:
{
  options.stylix.targets.helix.enable =
    config.lib.stylix.mkEnableTarget "Helix" true;

  config =
    lib.mkIf
      (
        config.stylix.enable
        && config.stylix.targets.helix.enable
        && config.programs.helix.enable
      )
      {
        programs.helix = {
          settings.theme = "stylix";
          themes.stylix = import ./theme.nix config;
        };
      };
}
