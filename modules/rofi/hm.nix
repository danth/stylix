{ pkgs, config, lib, ... }:

with config.stylix.fonts;

let
  rofiOpacity = builtins.toString (builtins.ceil (config.stylix.opacity.popups * 100));
  extraCss = ''
  * {
   background: rgba ( {{base00-rgb-r}}, {{base00-rgb-g}}, {{base00-rgb-b}}, ${rofiOpacity} % );
   lightbg: rgba ( {{base01-rgb-r}}, {{base01-rgb-g}}, {{base01-rgb-b}}, ${rofiOpacity} % );
  }
  '';
  themeFile = config.lib.stylix.colors {
    templateRepo = pkgs.fetchFromGitHub {
      owner = "tinted-theming";
      repo = "base16-rofi";
      rev = "3f64a9f8d8cb7db796557b516682b255172c4ab4";
      sha256 = "sha256-RZpjCQ8KGO3cv9A/lNNoTE+WJ9sNk5sz0zJq02zzxA8=";
    };
  }/default.mustache + extraCss;
in
{
  options.stylix.targets.rofi.enable =
    config.lib.stylix.mkEnableTarget "Rofi" true;

  config = lib.mkIf config.stylix.targets.rofi.enable {
    programs.rofi = {
      font = "${monospace.name} ${toString sizes.popups}";
      theme = themeFile;
    };
  };
}
