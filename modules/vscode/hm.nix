{
  pkgs,
  config,
  lib,
  ...
}@args:

with config.stylix.fonts;

let
  extension =
    pkgs.runCommandLocal "stylix-vscode"
      {
        vscodeExtUniqueId = "stylix.stylix";
        vscodeExtPublisher = "stylix";
        version = "0.0.0";
        theme = builtins.toJSON (import ./templates/theme.nix args);
        passAsFile = [ "theme" ];
      }
      ''
        mkdir -p "$out/share/vscode/extensions/$vscodeExtUniqueId/themes"
        ln -s ${./package.json} "$out/share/vscode/extensions/$vscodeExtUniqueId/package.json"
        cp "$themePath" "$out/share/vscode/extensions/$vscodeExtUniqueId/themes/stylix.json"
      '';

  settings = import ./templates/settings.nix args;

  profile = {
    extensions = [ extension ];
    userSettings = settings;
  };

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
    programs.vscode.profiles = lib.genAttrs cfg.profileNames (_name: profile);

    warnings =
      lib.optional (config.programs.vscode.enable && cfg.profileNames == [ ])
        ''stylix: vscode: `config.stylix.targets.vscode.profileNames` is not set. Declare profile names with 'config.stylix.targets.vscode.profileNames = [ "<PROFILE_NAME>" ];'.'';
  };
}
