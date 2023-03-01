{ inputs, pkgs }:

with pkgs.lib;

let
  makeSystem =
    module: _:
    let
      name = removeSuffix ".nix" module;

      configuration = inputs.nixpkgs.lib.nixosSystem {
        inherit (pkgs) system;

        modules = [
          inputs.home-manager.nixosModules.home-manager
          inputs.self.nixosModules.stylix
          ./common.nix
          ./configurations/${module}
          { system.name = name; }
        ];
      };

    in nameValuePair name configuration.config.system.build.vm;

in mapAttrs' makeSystem (builtins.readDir ./configurations)
