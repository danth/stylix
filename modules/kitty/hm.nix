{ config, lib, pkgs, ... }:

let
  cfg = config.stylix.targets.kitty;
  theme = config.lib.stylix.colors {
    templateRepo = pkgs.fetchFromGitHub {
      owner = "kdrag0n";
      repo = "base16-kitty";
      rev = "06bb401fa9a0ffb84365905ffbb959ae5bf40805";
      sha256 = "sha256-aRaizTYPpuWEcvoYE9U+YRX+Wsc8+iG0guQJbvxEdJY=";
    };
    target = if cfg.variant256Colors then "default-256" else "default";
  };
in {
  options.stylix.targets.kitty = {
    enable = config.lib.stylix.mkEnableTarget "Kitty" true;

    variant256Colors = lib.mkOption {
      description = lib.mdDoc ''
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
      settings.background_opacity = with config.stylix.opacity; "${builtins.toString terminal}";
      extraConfig = builtins.readFile theme;
    };
  };
}
