{ config, lib, ... }:

let
  cfg = config.stylix.targets.foot;

  theme = config.lib.stylix.colors {
    templateRepo = config.lib.stylix.templates.tinted-foot;
  };

in
{
  options.stylix.targets.foot.enable =
    config.lib.stylix.mkEnableTarget "Foot" true;

  config.programs.foot.settings = lib.mkIf cfg.enable {
    main = {
      include = theme;
      font =
        with config.stylix.fonts;
        "${monospace.name}:size=${toString sizes.terminal}";
      dpi-aware = "no";
    };
    colors.alpha = with config.stylix.opacity; terminal;
  };
}
