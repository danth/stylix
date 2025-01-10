{ config, lib, ... }:

with config.lib.stylix.colors;

{
  options.stylix.targets.console.enable =
    config.lib.stylix.mkEnableTarget "the Linux kernel console" true;

  config.console.colors =
    lib.mkIf (config.stylix.enable && config.stylix.targets.console.enable)
      [
        base00-hex
        red
        green
        yellow
        blue
        magenta
        cyan
        base05-hex
        base03-hex
        red
        green
        yellow
        blue
        magenta
        cyan
        base06-hex
      ];
}
