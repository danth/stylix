{ config, lib, ... }:

{
  options.stylix.targets.wpaperd.enable =
    config.lib.stylix.mkEnableTarget "wpaperd" true;

  config =
    lib.mkIf (config.stylix.enable && config.stylix.targets.wpaperd.enable)
      {
        programs.wpaperd.settings.any = {
          path = "${config.stylix.image}";
          mode =
            let
              inherit (config.stylix) imageScalingMode;
            in
            if imageScalingMode == "fill" then "fit" else imageScalingMode;
        };
      };
}
