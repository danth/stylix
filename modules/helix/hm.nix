{ pkgs, config, lib, ... }:

let
  theme = config.lib.stylix.colors {
    templateRepo = pkgs.fetchFromGitHub {
      owner = "tinted-theming";
      repo = "base16-helix";
      rev = "3e391da3aaf2fa43dc9cfb67e4e6216c7ce163dc";
      sha256 = "elhPirlFx9UaZGTht9jFl8lLfT4Af38ITTPJT90yrHs=";
    };
  };

  # Removing the background exposes transparency from the terminal. The
  # background might be helpful if the terminal isn't themed, so we only
  # do this if transparency is actually enabled.
  transparentTheme = pkgs.runCommandLocal "helix-transparent.toml" {} ''
    sed 's/,\? bg = "base00"//g' <${theme} >$out
  '';

in {
  options.stylix.targets.helix.enable =
    config.lib.stylix.mkEnableTarget "Helix" config.programs.helix.enable;

  config = lib.mkIf config.stylix.targets.helix.enable {
    programs.helix.settings.theme = "stylix";

    xdg.configFile."helix/themes/stylix.toml".source =
      if config.stylix.opacity.terminal == 1.0
      then theme
      else transparentTheme;
  };
}
