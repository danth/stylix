{ mkTarget, lib, ... }:
mkTarget {
  name = "kitty";
  humanName = "Kitty";

  extraOptions = {
    variant256Colors = lib.mkOption {
      description = ''
        Whether to use the [256-color variant](https://github.com/kdrag0n/base16-kitty#256-color-variants)
        rather than the default combination of colors.
      '';
      type = lib.types.bool;
      default = false;
    };
  };

  configElements = [
    (
      { fonts }:
      {
        programs.kitty.font = {
          inherit (fonts.monospace) package name;
          size = fonts.sizes.terminal;
        };
      }
    )
    (
      { opacity }:
      {
        programs.kitty.settings.background_opacity = toString opacity.terminal;
      }
    )
    (
      {
        cfg,
        colors,
        inputs,
      }:
      let
        theme = colors {
          templateRepo = inputs.tinted-kitty;
          target = if cfg.variant256Colors then "base16-256-deprecated" else "base16";
        };
      in
      {
        programs.kitty.extraConfig = ''
          include ${theme}
        '';
      }
    )
  ];
}
