{ lib, ... }:

{
  options.stylix.opacity = {
    desktop = lib.mkOption {
      description = "The opacity of the windows of bars/widgets, the amount of applications supported is currently limited";
      type = lib.types.float;
      default = 1.0;
    };
    applications = lib.mkOption {
      description = "The opacity of the windows of applications, the amount of applications supported is currently limited";
      type = lib.types.float;
      default = 1.0;
    };
    terminal = lib.mkOption {
      description = "The opacity of the windows of terminals, this works across all terminals supported by stylix";
      type = lib.types.float;
      default = 1.0;
    };
    popups = lib.mkOption {
      description = "The opacity of the windows of notifications/popups, the amount of applications supported is currently limited";
      type = lib.types.float;
      default = 1.0;
    };
  };
}
