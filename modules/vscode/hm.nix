{
  mkTarget,
  pkgs,
  config,
  lib,
  ...
}:

mkTarget {
  name = "vscode";
  humanName = "VSCode";

  extraOptions.profileNames = lib.mkOption {
    description = "The VSCode profile names to apply styling on.";
    type = lib.types.listOf lib.types.str;
    default = [ "default" ];
  };

  configElements = [
    (
      { cfg }:
      {
        warnings =
          lib.optional (config.programs.vscode.enable && cfg.profileNames == [ ])
            ''stylix: vscode: `config.stylix.targets.vscode.profileNames` is empty. No theming will be applied. Add a profile or disable this warning by setting `stylix.targets.vscode.enable = false`.'';
      }
    )
    (
      { cfg, colors }:
      {
        programs.vscode.profiles = lib.genAttrs cfg.profileNames (_: {
          extensions = lib.singleton (
            pkgs.runCommandLocal "stylix-vscode"
              {
                vscodeExtUniqueId = "stylix.stylix";
                vscodeExtPublisher = "stylix";
                version = "0.0.0";
                theme = builtins.toJSON (import ./templates/theme.nix colors);
                passAsFile = [ "theme" ];
              }
              ''
                mkdir -p "$out/share/vscode/extensions/$vscodeExtUniqueId/themes"
                ln -s ${./package.json} "$out/share/vscode/extensions/$vscodeExtUniqueId/package.json"
                cp "$themePath" "$out/share/vscode/extensions/$vscodeExtUniqueId/themes/stylix.json"
              ''
          );
        });
      }
    )
    (
      { cfg, fonts }:
      {
        programs.vscode.profiles = lib.genAttrs cfg.profileNames (_: {
          userSettings = import ./templates/settings.nix fonts;
        });
      }
    )
  ];
}
