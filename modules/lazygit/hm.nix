{ mkTarget, ... }:
mkTarget {
  name = "lazygit";
  humanName = "lazygit";

  configElements =
    { colors }:
    {
      programs.lazygit.settings.gui.theme = with colors.withHashtag; {
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
