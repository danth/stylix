{ lib, ... }:
{
  name = "VSCode";
  homepage = "https://code.visualstudio.com/";
  maintainers = with lib.maintainers; [
    Flameopathic
    danth
  ];
  description = ''
    When theming is enabled for VSCode, Stylix will create and apply its own
    VSCode theme, as well as configure settings concerning fonts.
    > [!TIP]
    > The theme created is called
    > [`Stylix`](
    > https://github.com/nix-community/stylix/blob/master/modules/vscode/templates/theme.nix),
    > and if one so desires, its colors can be changed using
    > `programs.vscode.profiles.<name>.userSettings`
    > (exactly as one would customize any other VSCode theme).
    > See the example below; the syntax is explained in the
    > [VSCode documentation](
    > https://code.visualstudio.com/docs/configure/themes#_customize-a-color-theme).
    > ```nix
    > programs.vscode.profiles.default.userSettings = {
    >   "workbench.colorCustomizations" = {
    >     "[Stylix]" = {
    >       "editor.wordHighlightBackground"  = "#''${colors.base02}66";
    >       "editor.hoverHighlightBackground" = "#''${colors.base02}66";
    >     };
    >   };
    > };
    > ```
  '';
}
