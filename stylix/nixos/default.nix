{ palette-generator, base16 }@args:
{ config, lib, ... }:

let
  cfg = config.stylix.home-manager;
in {
  imports = [
    ../pixel.nix
    ../target.nix
    ./fonts.nix
    (import ./palette.nix { inherit palette-generator base16; })
  ];

  options.stylix.home-manager = {
    enable = lib.mkEnableOption "home-manager support";
    useSystemTheme = lib.mkEnableOption "system theme in home-manager";
  };

  config = lib.mkIf cfg.enable {
    home-manager.sharedModules = [
      { stylix.useSystemTheme = lib.mkOverride 99 cfg.useSystemTheme; }
    ];
  };
}
