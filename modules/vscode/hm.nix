{
  pkgs,
  config,
  lib,
  ...
}:

with config.stylix.fonts;

let
  themeFile = config.lib.stylix.colors {
    template = ./template.mustache;
    extension = ".json";
  };

  themeExtension =
    pkgs.runCommandLocal "stylix-vscode"
      {
        vscodeExtUniqueId = "stylix.stylix";
        vscodeExtPublisher = "stylix";
        version = "0.0.0";
      }
      ''
        mkdir -p "$out/share/vscode/extensions/$vscodeExtUniqueId/themes"
        ln -s ${./package.json} "$out/share/vscode/extensions/$vscodeExtUniqueId/package.json"
        ln -s ${themeFile} "$out/share/vscode/extensions/$vscodeExtUniqueId/themes/stylix.json"
      '';

  cfg = config.stylix.targets.vscode;

in
{
  options.stylix.targets.vscode = {
    enable = config.lib.stylix.mkEnableTarget "VSCode" true;
    profileNames = lib.mkOption {
      description = "The VSCode profile names to apply styling on.";
      type = lib.types.listOf lib.types.str;
      default = [ ];
    };
  };

  config = lib.mkIf (config.stylix.enable && cfg.enable) {
    programs.vscode.profiles = lib.mkMerge (
      map (profileName: {
        ${profileName} = {
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
            "editor.fontSize" = sizes.terminal * 4.0 / 3.0;
            "debug.console.fontSize" = sizes.terminal * 4.0 / 3.0;
            "markdown.preview.fontSize" = sizes.terminal * 4.0 / 3.0;
            "terminal.integrated.fontSize" = sizes.terminal * 4.0 / 3.0;
            "chat.editor.fontSize" = sizes.terminal * 4.0 / 3.0;

            # other factors (9/14, 13/14, 56/14) based on default for given value
            # divided by default for `editor.fontSize` (14) from
            # https://code.visualstudio.com/docs/getstarted/settings#_default-settings.
            "editor.minimap.sectionHeaderFontSize" =
              sizes.terminal * 4.0 / 3.0 * 9.0 / 14.0;
            "scm.inputFontSize" = sizes.terminal * 4.0 / 3.0 * 13.0 / 14.0;
            "screencastMode.fontSize" = sizes.terminal * 4.0 / 3.0 * 56.0 / 14.0;
          };
        };
      }) cfg.profileNames
    );
    warnings =
      lib.optional
        (
          config.programs.vscode.enable
          && config.stylix.targets.vscode.profileNames == [ ]
        )
        ''stylix: vscode: `config.stylix.targets.vscode.profileNames` is not set. Declare profile names with 'config.stylix.targets.vscode.profileNames = [ "<PROFILE_NAME>" ];'.'';
  };
}
