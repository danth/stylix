{ config, lib, ... }:
let
  c = color: "0x${color}";
  cfg = config.stylix.targets.wayprompt;
in
{
  options.stylix.targets.wayprompt.enable =
    config.lib.stylix.mkEnableTarget "wayprompt" true;

  config = lib.mkIf (config.stylix.enable && cfg.enable) {
    programs.wayprompt.settings.colours = with config.lib.stylix.colors; {
      background = c base00;
      border = c base0D;
      text = c base05;
      error-text = c base08;

      pin-background = c base01;
      pin-border = c base05;
      pin-square = c base05;

      ok-button = c base0B; # green
      ok-button-border = c base05;
      ok-button-text = c base00;

      not-ok-button = c base0A; # yellow
      not-ok-button-border = c base05;
      not-ok-button-text = c base00;

      cancel-button = c base08; # red
      cancel-button-border = c base05;
      cancel-button-text = c base00;
    };
  };
}
