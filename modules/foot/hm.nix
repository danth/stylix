{ config, lib, ... }:
let
  theme = config.lib.stylix.colors {
    templateRepo = config.stylix.inputs.tinted-foot;
  };
in
{
  options.stylix.targets.foot.enable =
    config.lib.stylix.mkEnableTarget "Foot" true;

  config.programs.foot.settings = lib.mkIf config.stylix.targets.foot.enable {
    main = {
      include = theme;
      font =
        with config.stylix.fonts;
        "${monospace.name}:size=${toString sizes.terminal}";
      dpi-aware = "no";
    };
    colors.alpha = config.stylix.opacity.terminal;
  };
}
