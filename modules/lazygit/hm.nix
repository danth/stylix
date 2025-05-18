{
  config,
  lib,
  ...
}:
{
  options.stylix.targets.lazygit.enable =
    config.lib.stylix.mkEnableTarget "lazygit" true;

  config =
    lib.mkIf (config.stylix.enable && config.stylix.targets.lazygit.enable)
      {
        programs.lazygit.settings.gui.theme =
          with config.lib.stylix.colors.withHashtag; {
            activeBorderColor = [
              base07
              "bold"
            ];
            inactiveBorderColor = [ base04 ];
            searchingActiveBorderColor = [
              base02
              "bold"
            ];
            optionsTextColor = [ base06 ];
            selectedLineBgColor = [ base03 ];
            cherryPickedCommitBgColor = [ base02 ];
            cherryPickedCommitFgColor = [ base03 ];
            unstagedChangesColor = [ base08 ];
            defaultFgColor = [ base05 ];
          };
      };
}
