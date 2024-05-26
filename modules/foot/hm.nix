{ pkgs, config, lib, ... }:

let
  cfg = config.stylix.targets.foot;

  theme = config.lib.stylix.colors {
    templateRepo = config.lib.stylix.templates.base16-foot;
  };

in {
  options.stylix.targets.foot.enable =
    config.lib.stylix.mkEnableTarget "Foot" true;

  config.programs.foot.settings = lib.mkIf cfg.enable {
    main = {
        include = theme;
        font = let
            inherit (config.stylix) fonts;
            inherit  (fonts) sizes;
            monospace = builtins.head fonts.monospace;
          in
          "${monospace.name}:size=${toString sizes.terminal}";
        dpi-aware = "no";
    };
    colors.alpha = with config.stylix.opacity; terminal;
  };
}
