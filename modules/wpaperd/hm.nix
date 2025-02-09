{ config, lib, ... }:
let
  cfg = config.stylix.targets.wpaperd;
in
{
  options.stylix.targets.wpaperd = {
    enable = config.lib.stylix.mkEnableTarget "wpaperd" true;
    useWallpaper = config.lib.stylix.mkEnableWallpaper "wpaperd" true;
  };

  config = lib.mkIf (config.stylix.enable && cfg.enable) (
    let
      inherit (config.stylix) imageScalingMode;

      # wpaperd doesn't have any mode close to the described behavior of center
      modeMap = {
        "stretch" = "stretch";
        # wpaperd's center mode is closest to the described behavior of fill
        "fill" = "center";
        "fit" = "fit";
        "tile" = "tile";
      };

      modeAttrs =
        if builtins.hasAttr imageScalingMode modeMap then
          { mode = modeMap.${imageScalingMode}; }
        else
          lib.info "stylix: wpaperd: unsupported image scaling mode: ${imageScalingMode}"
            { };
    in
    {
      programs.wpaperd.settings.any =
        lib.mkIf cfg.useWallpaper {
          path = "${config.stylix.image}";
        }
        // modeAttrs;
    }
  );
}
