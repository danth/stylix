{ config, lib, ... }:

with config.lib.stylix.colors;

let
  opacity = lib.toHexString (builtins.ceil (config.stylix.opacity.popups * 255));

in
{
  options.stylix.targets.fuzzel.enable =
    config.lib.stylix.mkEnableTarget "Fuzzel" true;

  config.programs.fuzzel.settings =
    lib.mkIf (config.stylix.enable && config.stylix.targets.fuzzel.enable)
      {
        colors = {
          background = "${base00-hex}${opacity}";
          text = "${base05-hex}ff";
          placeholder = "${base03-hex}ff";
          prompt = "${base05-hex}ff";
          input = "${base05-hex}ff";
          match = "${base0A-hex}ff";
          selection = "${base03-hex}ff";
          selection-text = "${base05-hex}ff";
          selection-match = "${base0A-hex}ff";
          counter = "${base06-hex}ff";
          border = "${base0D-hex}ff";
        };

        main = {
          font = "${config.stylix.fonts.sansSerif.name}:size=${toString config.stylix.fonts.sizes.popups}";
        };
      };
}
