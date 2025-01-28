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
  config = lib.mkIf config.stylix.enable {
    terminal.colors = lib.getAttrs colorAttrs (
      fromKittyConf (builtins.readFile theme)
    );
  };
}
