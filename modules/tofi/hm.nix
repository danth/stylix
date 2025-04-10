{
  config,
  lib,
  ...
}:
{
  options.stylix.targets.tofi.enable =
    config.lib.stylix.mkEnableTarget "Tofi" true;

  config = lib.mkIf (config.stylix.enable && config.stylix.targets.tofi.enable) {
    programs.tofi.settings =
      with config.lib.stylix.colors.withHashtag;
      let
        inherit (config.stylix) fonts;
        opacity = lib.toHexString (
          ((builtins.ceil (config.stylix.opacity.popups * 100)) * 255) / 100
        );
        background = base00 + opacity;
        foreground = base05;
        darkForeground = base04 + opacity;
        selection = base03 + opacity;
      in
      {
        font = fonts.monospace.name;
        font-size = toString fonts.sizes.popups;
        background-color = background;
        outline-color = darkForeground;
        border-color = foreground;
        text-color = foreground;
        prompt-color = base0A;
        prompt-background = background;
        placeholder-color = selection;
        input-background = background;
        default-result-background = background;
        selection-color = selection;
        selection-background = background;
        border-width = lib.mkDefault 4;
        outline-width = lib.mkDefault 2;
      };
  };
}
