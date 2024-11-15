{
  config,
  lib,
  ...
}:
with config.stylix.fonts;
with config.lib.stylix.colors.withHashtag;
{
  options.stylix.targets.tofi.enable =
    config.lib.stylix.mkEnableTarget "Tofi" true;

  config = lib.mkIf (config.stylix.enable && config.stylix.targets.tofi.enable) {
    programs.tofi.settings =
      let
        opacity = lib.toHexString (
          ((builtins.ceil (config.stylix.opacity.popups * 100)) * 255) / 100
        );
        background = base00 + opacity;
        foreground = base05;
        darkForeground = base04 + opacity;
        selection = base03 + opacity;
      in
      {
        font = monospace.name;
        font-size = toString sizes.popups;
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
