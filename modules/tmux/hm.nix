{ pkgs, config, lib, ... }:

let
  theme = config.lib.stylix.colors {
    templateRepo = config.lib.stylix.templates.base16-tmux;
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
