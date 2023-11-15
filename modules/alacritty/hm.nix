{pkgs, config, lib, ... }:

with config.stylix.fonts;

let
  themeFile = config.lib.stylix.colors {
    templateRepo = config.lib.stylix.templates.base16-alacritty;
  };
in
{
  options.stylix.targets.alacritty.enable =
    config.lib.stylix.mkEnableTarget "Alacritty" true;

  config = lib.mkIf config.stylix.targets.alacritty.enable {
    programs.alacritty.settings = {
      font = {
        normal = {
          family = monospace.name;
          style = "Regular";
        };
        size = sizes.terminal;
      };
      window.opacity = with config.stylix.opacity; terminal;
      import = [ themeFile ];
    };
  };
}
