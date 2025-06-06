{ lib, ... }:
{
  name = "VSCode";
  homepage = "https://code.visualstudio.com/";
  maintainers = with lib.maintainers; [
    Flameopathic
    danth
  ];
  description = ''
    > [!TIP]
    > Colors can be overridden using `programs.vscode.profiles.<name>.userSettings`. See the [Visual Studio Code docs](https://code.visualstudio.com/docs/configure/themes#_customize-a-color-theme).
  '';
}
