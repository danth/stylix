{ config, lib, ... }:

let
  fromKittyConf =
    conf:
    builtins.listToAttrs (
      map (
        line:
        lib.nameValuePair (lib.head line) (
          builtins.concatStringsSep " " (builtins.tail line)
        )
      ) (map (lib.splitString " ") (lib.splitString "\n" conf))
    );
  theme = config.lib.stylix.colors {
    templateRepo = config.lib.stylix.templates.tinted-kitty;
    target = "default";
  };
  colorAttrs = [
    "background"
    "foreground"
    "cursor"
  ] ++ (map (n: "color${toString n}") (lib.range 0 15));
in
{
  options.stylix.targets.terminal.enable =
    config.lib.stylix.mkEnableTarget "terminal" true;

  config = lib.mkIf config.stylix.enable {
    terminal.colors = lib.getAttrs colorAttrs (
      fromKittyConf (builtins.readFile theme)
    );

    android-integration.termux-reload-settings.enable = true;
    build.activationAfter.reloadTermuxSettings = ''
      $DRY_RUN_CMD termux-reload-settings
    '';
  };
}
