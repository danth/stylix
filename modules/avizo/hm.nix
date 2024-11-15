{
  config,
  lib,
  options,
  ...
}:

with config.lib.stylix.colors;
with config.stylix.fonts;
let
  aviOpacity = toString config.stylix.opacity.popups;
in
{
  options.stylix.targets.avizo.enable =
    config.lib.stylix.mkEnableTarget "Avizo" true;

  # Referenced https://github.com/stacyharper/base16-mako
  config = lib.optionalAttrs (options.services ? avizo) (
    lib.mkIf (config.stylix.enable && config.stylix.targets.avizo.enable) {
      services.avizo = {
        settings = {
          default = {
            background = "rgba(${base01-rgb-r}, ${base01-rgb-g}, ${base01-rgb-b}, ${aviOpacity})";
            border-color = "rgba(${base0D-rgb-r}, ${base0D-rgb-g}, ${base0D-rgb-b}, ${aviOpacity})";
            bar-fg-color = "rgba(${base05-rgb-r}, ${base05-rgb-g}, ${base05-rgb-b}, ${aviOpacity})";
            bar-bg-color = "rgba(${base01-rgb-r}, ${base01-rgb-g}, ${base01-rgb-b}, ${aviOpacity})";
            image-opacity = aviOpacity;
          };
        };
      };
    }
  );
}
