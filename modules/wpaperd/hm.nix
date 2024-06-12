{ config, lib, ... }:

{
  options.stylix.targets.wpaperd.enable = config.lib.stylix.mkEnableTarget "wpaperd" true;

  config = lib.mkIf (config.stylix.enable && config.stylix.targets.wpaperd.enable) {
    programs.wpaperd.settings.any.path = "${config.stylix.image}";
  };
}
