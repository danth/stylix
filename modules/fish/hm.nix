{ config, lib, ... }:

{
  options.stylix.targets.fish.enable =
    config.lib.stylix.mkEnableTarget "Fish" true;

  config = lib.mkIf (config.stylix.enable && config.stylix.targets.fish.enable) {
    programs.fish.interactiveShellInit = import ./prompt.nix config;
  };
}
