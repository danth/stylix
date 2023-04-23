{pkgs, config, lib, ... }:

let
  themeFile = config.lib.stylix.colors {
    template = builtins.readFile ./template.mustache;
    extension = ".json";
  };

  themeExtension = pkgs.runCommandLocal "stylix-vscode" {
    vscodeExtUniqueId = "stylix.stylix";
    vscodeExtPublisher = "stylix";
    version = "0.0.0";
  } ''
    mkdir -p "$out/share/vscode/extensions/$vscodeExtUniqueId/themes"
    ln -s ${./package.json} "$out/share/vscode/extensions/$vscodeExtUniqueId/package.json"
    ln -s ${themeFile} "$out/share/vscode/extensions/$vscodeExtUniqueId/themes/stylix.json"
  '';

in {
  options.stylix.targets.vscode.enable =
    config.lib.stylix.mkEnableTarget "VSCode" true;

  config = lib.mkIf config.stylix.targets.vscode.enable {
    programs.vscode = {
      extensions = [ themeExtension ];
      userSettings."workbench.colorTheme" = "Stylix";
    };
  };
}

