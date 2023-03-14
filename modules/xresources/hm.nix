{ pkgs, config, lib, ... }:

with config.stylix.fonts;

let
  themeFile = config.lib.stylix.colors {
    templateRepo = pkgs.fetchFromGitHub {
      owner = "tinted-theming";
      repo = "base16-xresources";
      rev = "6711cf4fa61e903e52ef6eac186b83e04a0397d8";
      sha256 = "sha256-WazWviLhQRCyF9EBi2IDn9h8wrKc00PpqtltFDpUP5Q=";
    };
  };
in
{
  options.stylix.targets.xresources.enable =
    config.lib.stylix.mkEnableTarget "Xresources" true;

  config = lib.mkIf config.stylix.targets.xresources.enable {
    xresources = {
      properties = {
        "*.faceName" = monospace.name;
        "*.faceSize" = sizes.terminal;
        "*.renderFont" = true;
      };
      extraConfig = builtins.readFile themeFile;
    };
  };
}
