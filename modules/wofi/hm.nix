{
  config,
  lib,
  ...
}:
{
  options.stylix.targets.wofi.enable =
    config.lib.stylix.mkEnableTarget "wofi" config.programs.wofi.enable;

  config = lib.mkIf (config.stylix.enable && config.stylix.targets.wofi.enable) {
    programs.wofi.style =
      let
        inherit (config.stylix) fonts;
      in
      with config.lib.stylix.colors.withHashtag;
      ''
        window {
          font-family: "${fonts.monospace.name}";
          font-size: ${toString fonts.sizes.popups}pt;

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
