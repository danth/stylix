{ pkgs, config, lib, ... } @ args:

with lib;

let
  cfg = config.stylix.opacity;
  fromOs = import ./fromos.nix { inherit lib args; };
in {
    options.stylix.opacity = {
        desktop = mkOption {
            description = mkDoc "The opacity of the windows of bars/widgets";
            type = types.float;
            default = fromOs [ "opacity" "desktop" ] 1.0;
        };
        applications = mkOption {
            description = mkDoc "The opacity of the windows of applications";
            type = types.float;
            default = fromOs [ "opacity" "applications" ] 1.0;
        };
        terminal = mkOption {
            description = mkDoc "The opacity of the windows of terminals";
            type = types.float;
            default = fromOs [ "opacity" "terminal" ] 1.0;
        };
        popups = mkOption {
            description = mkDoc "The opacity of the windows of notifications/popups";
            type = types.float;
            default = fromOs [ "opacity" "popups" ] 1.0;
        };
    };
}
