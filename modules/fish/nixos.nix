{ pkgs, config, lib, ... }:

{
  options.stylix.targets.fish.enable =
    config.lib.stylix.mkEnableTarget "Fish" true;

  config = lib.mkIf config.stylix.targets.fish.enable {
    programs.fish.promptInit = import ./prompt.nix { inherit pkgs config; };
  };
}
