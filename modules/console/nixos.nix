{ config, lib, ... }:

with config.lib.stylix.colors;

{
  options.stylix.targets.console.enable =
    config.lib.stylix.mkEnableTarget "the Linux kernel console" true;

  config.console.colors = lib.mkIf config.stylix.targets.console.enable [
    base00-hex
    base08-hex
    base0B-hex
    base0A-hex
    base0D-hex
    base0E-hex
    base0C-hex
    base05-hex
    base03-hex
    base09-hex
    base01-hex
    base02-hex
    base04-hex
    base06-hex
    base0F-hex
    base07-hex
  ];
}
