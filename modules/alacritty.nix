{pkgs, config, lib, ... }:
let
  themeFile = config.lib.stylix.colors {
    templateRepo = pkgs.fetchFromGitHub {
      owner = "aarowill";
      repo = "base16-alacritty";
      rev = "914727e48ebf3eab1574e23ca0db0ecd0e5fe9d0";
      sha256 = lib.fakeSha256;
    };
  };
in
{
  home-manager.sharedModules = [{
    programs.alacritty.settings.import = [ themeFile ];
  };
}
