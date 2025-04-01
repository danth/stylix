{ config, lib, ... }:

{
    options.stylix.targets.freecad.enable = config.lib.stylix.mkEnableTarget "freecad" true; #(builtins.elem (pkgs.freecad) config.home.packages);

    config = lib.mkIf config.stylix.targets.freecad.enable {
        xdg.dataFile = {
            "FreeCAD/Mod/Stylix/Stylix/Stylix.cfg".text = import ./cfg.nix config lib;
            "FreeCAD/Mod/Stylix/Stylix/Stylix.qss".text = import ./stylesheet.nix config;
            "FreeCAD/Mod/Stylix/package.xml".text = import ./package.nix config;
        };
    };

}
