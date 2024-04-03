{ pkgs, inputs, lib, ... }:

let
  commonModule = { config, ... }: {
    users.users.testbed = {
      description = "Testbed";
      hashedPassword = "";
      isNormalUser = true;
    };

    # The state version can safely track the latest release because the disk
    # image is ephermal.
    system.stateVersion = config.system.nixos.release;
    home-manager.users.testbed.home.stateVersion = config.system.nixos.release;

    virtualisation.vmVariant.virtualisation = {
      # This is a maximum limit; the VM should still work if the host has fewer cores.
      cores = 4;
      memorySize = 2048;
    };
  };

  autoload = builtins.concatLists
    (lib.mapAttrsToList
      (name: _:
        let testbed = {
          inherit name;
          module = "${../modules}/${name}/testbed.nix";
        };
        in
          lib.optional (builtins.pathExists testbed.module) testbed
      )
      (builtins.readDir ../modules));

  makeTestbed =
    testbed: stylix:
    let
      name = "testbed-${testbed.name}-${stylix.polarity}";

      system = lib.nixosSystem {
        inherit (pkgs) system;
        modules = [
          inputs.self.nixosModules.stylix
          inputs.home-manager.nixosModules.home-manager
          commonModule
          testbed.module
          {
            inherit stylix;
            system.name = name;
          }
        ];
      };

      script = pkgs.writeShellApplication {
        inherit name;
        text = ''
          directory="''${XDG_CACHE_HOME:-$HOME/.cache}/stylix-testbed"
          mkdir --parents "$directory"

          # The disk image is only initialised if the file doesn't already exist,
          # so we need to create a temporary directory and give a path within it
          # rather than creating a temporary file. 
          directory="$(mktemp --directory "$directory/XXXXXXXXXX")"

          NIX_DISK_IMAGE="$directory/nixos.qcow2"
          export NIX_DISK_IMAGE

          clean() {
            if rm --recursive "$directory"; then
              echo 'Virtualisation disk image removed.'
            fi
          }
          trap clean EXIT

          ${lib.getExe system.config.system.build.vm}
        '';
      };
    in
      lib.nameValuePair name script;

  makeTestbeds = testbed: map (makeTestbed testbed) [
    {
      image = "${pkgs.pantheon.elementary-wallpapers}/share/backgrounds/Photo of Valley.jpg";
      base16Scheme = "${pkgs.base16-schemes}/share/themes/catppuccin-latte.yaml";
      polarity = "light";
    }
    {
      image = "${pkgs.pantheon.elementary-wallpapers}/share/backgrounds/Snow-Capped Mountain.jpg";
      base16Scheme = "${pkgs.base16-schemes}/share/themes/catppuccin-macchiato.yaml";
      polarity = "dark";
    }
  ];

in
  lib.listToAttrs (lib.flatten (map makeTestbeds autoload))
