{ config, lib, ... }:
{
  options.stylix.targets.cava = {
    enable = config.lib.stylix.mkEnableTarget "CAVA" true;
    rainbow.enable = lib.mkEnableOption "rainbow gradient theming";
  };

  config =
    let
      cfg = config.stylix.targets.cava;

      mkGradient =
        colors:
        lib.listToAttrs (
          lib.imap0 (
            i: c: lib.nameValuePair "gradient_color_${toString (i + 1)}" "'#${c}'"
          ) colors
        )
        // {
          gradient = 1;
          gradient_count = builtins.length colors;
        };
    in
    lib.mkIf (config.stylix.enable && cfg.enable) {
      programs.cava.settings.color = lib.mkIf cfg.rainbow.enable (
        mkGradient (
          with config.lib.stylix.colors;
          [
            base0E
            base0D
            base0C
            base0B
            base0A
            base09
            base08
          ]
        )
      );
    };
}
