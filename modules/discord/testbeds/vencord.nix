{ lib, pkgs, ... }:

let
  package = pkgs.discord.override {
    withVencord = true;
  };
in
{
  stylix.testbed.ui.application = {
    name = "discord";
    inherit package;
  };

  environment.systemPackages = [ package ];
  nixpkgs.config.allowUnfreePredicate =
    pkg:
    builtins.elem (lib.getName pkg) [
      "discord"
    ];
}
