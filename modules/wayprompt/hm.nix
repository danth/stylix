{ config, lib, ... }:
let
  cfg = config.stylix.targets.wayprompt;

  opacity = lib.toHexString (builtins.ceil (config.stylix.opacity.popups * 255));
in
{
  options.stylix.targets.wayprompt.enable =
    config.lib.stylix.mkEnableTarget "Wayprompt" true;

  config = lib.mkIf (config.stylix.enable && cfg.enable) {
    programs.wayprompt.settings.colours = with config.lib.stylix.colors; {
      background = "${base00-hex}${opacity}";
      border = base0D;
      text = base05;
      error-text = base08;

      pin-background = base01;
      pin-border = base05;
      pin-square = base05;

      ok-button = green;
      ok-button-border = green;
      ok-button-text = base00;

      not-ok-button = yellow;
      not-ok-button-border = yellow;
      not-ok-button-text = base00;

      cancel-button = red;
      cancel-button-border = red;
      cancel-button-text = base00;
    };
  };
}
