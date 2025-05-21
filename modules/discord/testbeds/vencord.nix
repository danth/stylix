{ lib, pkgs, ... }:

let
  package = pkgs.discord.override {
    withVencord = true;
  };
in
{
  stylix.testbed = {
    # Discord is not available on arm64.
    enable = lib.meta.availableOn pkgs.stdenv.hostPlatform package;

    ui.application = {
      name = "discord";
      inherit package;
    };
  };

  environment.systemPackages = [ package ];
  nixpkgs.config.allowUnfreePredicate =
    pkg:
    builtins.elem (lib.getName pkg) [
      "discord"
    ];
}
