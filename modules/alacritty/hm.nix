# Documentation is available at:
# - https://alacritty.org/config-alacritty.html
# - `man 5 alacritty`
{ config, lib, ... }:

let
  colors = config.lib.stylix.colors.withHashtag;
  useYaml = (builtins.compareVersions config.programs.alacritty.package.version "0.13.0") < 0;
  templateRepo = config.lib.stylix.templates.
    "base16-alacritty${if useYaml then "-yaml" else ""}";

  inherit (config.stylix) fonts;
  inherit (fonts) sizes;

  monospace = builtins.head fonts.monospace;

  themeFile = config.lib.stylix.colors {
    inherit templateRepo;
  };
in
{
  options.stylix.targets.alacritty.enable = config.lib.stylix.mkEnableTarget "Alacritty" true;

  config = lib.mkIf config.stylix.targets.alacritty.enable {
    programs.alacritty.settings = {
      font = with config.stylix.fonts; {
        normal = {
          family = monospace.name;
          style = "Regular";
        };
        size = sizes.terminal;
      };
      window.opacity = with config.stylix.opacity; terminal;
      colors = with colors; {
        primary = {
          foreground = base05;
          background = base00;
          bright_foreground = base07;
        };
        selection = {
          text = base05;
          background = base02;
        };
        cursor = {
          text = base00;
          cursor = base05;
        };
        normal = {
          black = base00;
          white = base05;
          inherit red green yellow blue magenta cyan;
        };
        bright = {
          black = base03;
          white = base07;
          red = bright-red;
          green = bright-green;
          yellow = yellow;
          blue = bright-blue;
          magenta = bright-magenta;
          cyan = bright-cyan;
        };
      };
    };
  };
}
