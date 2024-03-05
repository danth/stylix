{ pkgs, config, lib, ... }:

{
  options.stylix.targets.rofi.enable =
    config.lib.stylix.mkEnableTarget "Rofi" true;

  config.programs.rofi = lib.mkIf config.stylix.targets.rofi.enable {
    theme = config.stylix.colors {
      template =
        let
          opacity = config.lib.stylix.opacityToInt config.stylix.opacity.popups;
        in
        ''
          * {
            background: rgba ( {{base00-rgb-r}}, {{base00-rgb-g}}, {{base00-rgb-b}}, ${opacity} % );
            lightbg: rgba ( {{base01-rgb-r}}, {{base01-rgb-g}}, {{base01-rgb-b}}, ${opacity} % );
            red: rgba ( {{base08-rgb-r}}, {{base08-rgb-g}}, {{base08-rgb-b}}, ${opacity} % );
            blue: rgba ( {{base0D-rgb-r}}, {{base0D-rgb-g}}, {{base0D-rgb-b}}, ${opacity} % );
            lightfg: rgba ( {{base06-rgb-r}}, {{base06-rgb-g}}, {{base06-rgb-b}}, ${opacity} % );
            foreground: rgba ( {{base05-rgb-r}}, {{base05-rgb-g}}, {{base05-rgb-b}}, ${opacity} % );
          }
          ${builtins.readFile ./template.mustache}
        '';
      extension = ".rasi";
    };

    font = with config.stylix.fonts;
      "${monospace.name} ${toString sizes.popups}";
  };
}
