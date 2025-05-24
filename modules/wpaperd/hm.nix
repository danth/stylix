{ mkTarget, lib, ... }:
mkTarget {
  name = "wpaperd";
  humanName = "wpaperd";

  configElements =
    { imageScalingMode, image }:
    (
      let
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
        services.wpaperd.settings.any = {
          path = "${image}";
        } // modeAttrs;
      }
    );
}
