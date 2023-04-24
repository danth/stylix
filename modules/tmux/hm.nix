{ pkgs, config, lib, ... }:

let
  theme = config.lib.stylix.colors {
    templateRepo = pkgs.fetchFromGitHub {
        owner = "tinted-theming";
        repo = "base16-tmux";
        rev = "30fc84afc723e027d4497a284fcae3cb75097441";
        sha256 = "JJ/eRqTayuEKrL9MBe943HpKy7yLyd2Dmes58KN1jdk=";
    };
  };

in {
  options.stylix.targets.tmux.enable =
    config.lib.stylix.mkEnableTarget "Tmux" config.programs.tmux.enable;

  config = lib.mkIf config.stylix.targets.tmux.enable {
    programs.tmux.extraConfig = ''
    source-file ${theme}
    '';
  };
}
