{ pkgs, config, lib, ... }:

let
  style = config.lib.stylix.colors {
    template = builtins.readFile ./template.mustache;
    extension = "xml";
  };

in {
  options.stylix.targets.gedit.enable =
    config.lib.stylix.mkEnableTarget "GEdit" true;

  config = lib.mkIf config.stylix.targets.gedit.enable {
    home-manager.sharedModules = [{
      xdg.dataFile = {
        "gedit/styles/stylix.xml".source = style;
      };
    }];
  };
}
