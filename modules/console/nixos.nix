{ config, lib, ... }:

with config.lib.stylix.colors;

{
  options.stylix.targets.console.enable =
    config.lib.stylix.mkEnableTarget "the Linux kernel console" true;

  config.console.colors = lib.mkIf config.stylix.targets.console.enable [
    base00-hex
    red
    green
    yellow
    blue
    magenta
    cyan
    base07-hex
    base00-hex
    red
    green
    yellow
    blue
    magenta
    cyan
    base07-hex
  ];
}
