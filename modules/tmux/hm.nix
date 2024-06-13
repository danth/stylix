{ config, lib, ... }:

let
  theme = config.lib.stylix.colors {
    templateRepo = config.lib.stylix.templates.base16-tmux;
  };

in {
  options.stylix.targets.tmux.enable =
    config.lib.stylix.mkEnableTarget "Tmux" true;

  config = lib.mkIf (config.stylix.enable && config.stylix.targets.tmux.enable) {
    programs.tmux.extraConfig = ''
    source-file ${theme}
    '';
  };
}
