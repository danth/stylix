{ config, lib, ... }:

let 

  mkGradient = colors: lib.listToAttrs (lib.imap0 (i: c: lib.nameValuePair "gradient_color_${toString (i+1)}" "'#${c}'") colors) // {
    gradient = 1;
    gradient_count = builtins.length colors;
  };

  rainbowColors = with config.lib.stylix.colors; [
    base0E
    base0D
    base0C
    base0B
    base0A
    base09
    base08
  ];

in {
  options.stylix.targets.cava = {
    enable = config.lib.stylix.mkEnableTarget "CAVA" true;
    rainbow.enable = lib.mkEnableOption "rainbow gradient theming";
  };

  config = lib.mkIf (config.stylix.enable && config.stylix.targets.cava.enable) {
    programs.cava.settings.color = lib.mkIf config.stylix.targets.cava.rainbow.enable (mkGradient rainbowColors);
  };
}
