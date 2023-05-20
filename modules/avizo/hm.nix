{pkgs, config, lib, options, ... }:

with config.lib.stylix.colors.withHashtag;
with config.stylix.fonts;
let
  aviOpacity = lib.toHexString ((((builtins.ceil (config.stylix.opacity.popups * 100)) * 255) / 100));
in
{
  options.stylix.targets.avizo.enable =
    config.lib.stylix.mkEnableTarget "Avizo" true;

  # Referenced https://github.com/stacyharper/base16-mako
  config = lib.optionalAttrs (options.services ? avizo) (lib.mkIf config.stylix.targets.avizo.enable {
    services.avizo = {
        settings = {
            default = {
                background=base00 + aviOpacity;
                border-color=base0D;
                bar-fg-color=base05;
                bar-bg-color=base00 + aviOpacity;
            };
        };
    }; 
  });
}
