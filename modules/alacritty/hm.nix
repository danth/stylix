{pkgs, config, lib, ... }:

with config.stylix.fonts;

let
  useYaml = (builtins.compareVersions config.programs.alacritty.package.version "0.13.0") < 0;
  templateRepo = config.lib.stylix.templates.
    "base16-alacritty${if useYaml then "-yaml" else ""}";

  themeFile = config.lib.stylix.colors {
    inherit templateRepo;
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
