{
  config,
  lib,
  ...
}:
with config.stylix.fonts;
let
  colors = config.lib.stylix.colors.withHashtag;
in
{
  options.stylix.targets.wofi.enable =
    config.lib.stylix.mkEnableTarget "wofi" config.programs.wofi.enable;

  config = lib.mkIf (config.stylix.enable && config.stylix.targets.wofi.enable) {
    programs.wofi.style = with colors; ''
      window {
        font-family: "${monospace.name}";
        font-size: ${toString sizes.popups}pt;

        background-color: ${base00};
        color: ${base05};
      }

      #entry:nth-child(odd) {
        background-color: ${base00};
      }

      #entry:nth-child(even) {
        background-color: ${base01};
      }

      #entry:selected {
        background-color: ${base02};
      }

      #input {
        background-color: ${base01};
        color: ${base04};
        border-color: ${base02};
      }

      #input:focus {
        border-color: ${base0A};
      }
    '';
  };
}
