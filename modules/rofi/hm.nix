{ pkgs, config, lib, ... }:

with config.stylix.fonts;

let
  themeFile = config.lib.stylix.colors {
    templateRepo = pkgs.fetchFromGitHub {
      owner = "tinted-theming";
      repo = "base16-rofi";
      rev = "3f64a9f8d8cb7db796557b516682b255172c4ab4";
      sha256 = "sha256-RZpjCQ8KGO3cv9A/lNNoTE+WJ9sNk5sz0zJq02zzxA8=";
    };
  };
in
{
  options.stylix.targets.rofi.enable =
    config.lib.stylix.mkEnableTarget "Rofi" true;

  config = lib.mkIf config.stylix.targets.rofi.enable {
    home-manager.sharedModules = [{
      programs.rofi = {
        font = monospace.name;
        theme = themeFile;
      };
    }];
  };
}
