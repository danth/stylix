{ config, lib, ... }:

let
  cfg = config.stylix.targets.kitty;
  theme = config.lib.stylix.colors {
    templateRepo = config.stylix.inputs.tinted-kitty;
    target = if cfg.variant256Colors then "default-256" else "default";
  };
in
{
  options.stylix.targets.kitty = {
    enable = config.lib.stylix.mkEnableTarget "Kitty" true;

    variant256Colors = lib.mkOption {
      description = ''
        Whether to use the [256-color variant](https://github.com/kdrag0n/base16-kitty#256-color-variants)
        rather than the default combination of colors.
      '';
      type = lib.types.bool;
      default = false;
    };
  };

  config = lib.mkIf cfg.enable {
    programs.kitty = {
      font = {
        inherit (config.stylix.fonts.monospace) package name;
        size = config.stylix.fonts.sizes.terminal;
      };
      settings.background_opacity = "${builtins.toString config.stylix.opacity.terminal}";
      extraConfig = ''
        include ${theme}
      '';
    };
  };
}
