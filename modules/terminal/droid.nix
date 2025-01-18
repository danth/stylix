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
  ] ++ (map (n: "color${toString n}") (lib.range 1 15));
in
{
  options.stylix.targets.terminal.enable =
    config.lib.stylix.mkEnableTarget "terminal" true;

  config =
    lib.mkIf (config.stylix.enable && config.stylix.targets.terminal.enable)
      {
        terminal.colors = lib.getAttrs colorAttrs (
          fromKittyConf (builtins.readFile theme)
        );
      };
}
