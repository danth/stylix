{
  config,
  lib,
  ...
}:
let
  colors = config.lib.stylix.colors.withHashtag;
in
{
  options.stylix.targets.lazygit.enable =
    config.lib.stylix.mkEnableTarget "lazygit" true;

  config =
    lib.mkIf (config.stylix.enable && config.stylix.targets.lazygit.enable)
      {
        programs.lazygit.settings.gui.theme = {
          activeBorderColor = [
            colors.base07
            "bold"
          ];
          inactiveBorderColor = [ colors.base04 ];
          searchingActiveBorderColor = [
            colors.base02
            "bold"
          ];
          optionsTextColor = [ colors.base06 ];
          selectedLineBgColor = [ colors.base03 ];
          cherryPickedCommitBgColor = [ colors.base02 ];
          cherryPickedCommitFgColor = [ colors.base03 ];
          unstagedChangesColor = [ colors.base08 ];
          defaultFgColor = [ colors.base05 ];
        };
      };
}
