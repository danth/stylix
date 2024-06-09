{
  config,
  lib,
  ...
}:
with config.stylix.fonts;
with config.lib.stylix.colors.withHashtag; {
  options.stylix.targets.tofi.enable =
    config.lib.stylix.mkEnableTarget "Tofi" true;

  config = lib.mkIf (config.stylix.enable && config.stylix.targets.tofi.enable) {
    programs.tofi.settings = let
      background = base00;
      foreground = base05;
      darkForeground = base04;
      selection = base03;
    in {
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
    };
  };
}
