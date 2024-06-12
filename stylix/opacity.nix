{ lib, ... } @ args:

with lib;

{
    options.stylix.opacity = {
        desktop = mkOption {
            description = "The opacity of the windows of bars/widgets, the amount of applications supported is currently limited";
            type = types.float;
            default = 1.0;
        };
        applications = mkOption {
            description = "The opacity of the windows of applications, the amount of applications supported is currently limited";
            type = types.float;
            default = 1.0;
        };
        terminal = mkOption {
            description = "The opacity of the windows of terminals, this works across all terminals supported by stylix";
            type = types.float;
            default = 1.0;
        };
        popups = mkOption {
            description = "The opacity of the windows of notifications/popups, the amount of applications supported is currently limited";
            type = types.float;
            default = 1.0;
        };
    };
}
