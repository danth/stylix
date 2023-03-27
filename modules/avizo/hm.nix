{pkgs, config, lib, ... }:

with config.lib.stylix.colors.withHashtag;
with config.stylix.fonts;
{
  options.stylix.targets.avizo.enable =
    config.lib.stylix.mkEnableTarget "Avizo" true;

  # Referenced https://github.com/stacyharper/base16-mako
  config = lib.mkIf config.stylix.targets.avizo.enable {
    services.avizo = {
        settings = {
            default = {
                background=base00;
                border-color=base0D;
                bar-fg-color=base05;
                bar-bg-color=base00;
            };
        };
    }; 
  };
}
