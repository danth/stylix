{ config, lib, ... }:

{
  stylix.testbed.application = {
    enable = true;
    name = "steam";
    inherit (config.programs.steam) package;
  };

  programs.steam.enable = true;

  nixpkgs.config.allowUnfreePredicate =
    pkg:
    builtins.elem (lib.getName pkg) [
      "steam"
      "steam-unwrapped"
    ];
}
