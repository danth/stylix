{ pkgs, config, lib, ... } @ args:

with lib;

let
  cfg = config.stylix.opacity;
  fromOs = import ./fromos.nix { inherit lib args; };
in {
    options.stylix.opacity = {
        desktop = mkOption {
            description = mdDoc "The opacity of the windows of bars/widgets, the amount of applications supported is currently limited";
            type = types.float;
            default = fromOs [ "opacity" "desktop" ] 1.0;
        };
        applications = mkOption {
            description = mdDoc "The opacity of the windows of applications, the amount of applications supported is currently limited";
            type = types.float;
            default = fromOs [ "opacity" "applications" ] 1.0;
        };
        terminal = mkOption {
            description = mdDoc "The opacity of the windows of terminals, this works across all terminals supported by stylix";
            type = types.float;
            default = fromOs [ "opacity" "terminal" ] 1.0;
        };
        popups = mkOption {
            description = mdDoc "The opacity of the windows of notifications/popups, the amount of applications supported is currently limited";
            type = types.float;
            default = fromOs [ "opacity" "popups" ] 1.0;
        };
    };
}
