{pkgs, config, lib, ... }:

with config.stylix.fonts;

let
  themeFile = config.lib.stylix.colors {
    template = ./template.mustache;
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

  config = lib.mkIf (config.stylix.enable && config.stylix.targets.vscode.enable) {
    programs.vscode = {
      extensions = [ themeExtension ];
      userSettings = {
        "workbench.colorTheme" = "Stylix";
        "editor.fontFamily" = monospace.name;
        "editor.inlayHints.fontFamily" = monospace.name;
        "editor.inlineSuggest.fontFamily" = monospace.name;
        "scm.inputFontFamily" = monospace.name;
        "debug.console.fontFamily" = monospace.name;
        "markdown.preview.fontFamily" = sansSerif.name;
        "chat.editor.fontFamily" = monospace.name;

        # 4/3 factor used for pt to px;
        "editor.fontSize" = builtins.floor (sizes.terminal * 4 / 3 + 0.5);
        "debug.console.fontSize" = builtins.floor (sizes.terminal * 4 / 3 + 0.5);
        "markdown.preview.fontSize" = builtins.floor (sizes.terminal * 4 / 3 + 0.5);
        "terminal.integrated.fontSize" = builtins.floor (sizes.terminal * 4 / 3 + 0.5);
        "chat.editor.fontSize" = builtins.floor (sizes.terminal * 4 / 3 + 0.5);

        # other factors (9/14, 13/14, 56/14) based on default for given value
        # divided by default for `editor.fontSize` (14) from
        # https://code.visualstudio.com/docs/getstarted/settings#_default-settings.
        "editor.minimap.sectionHeaderFontSize" = builtins.floor (sizes.terminal * 4 / 3 * 9 / 14 + 0.5);
        "scm.inputFontSize" = builtins.floor (sizes.terminal * 4 / 3 * 13 / 14 + 0.5);
        "screencastMode.fontSize" = builtins.floor (sizes.terminal * 4 / 3 * 56 / 14 + 0.5);
      };
    };
  };
}

