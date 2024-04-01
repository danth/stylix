{ pkgs, inputs, lib, ... }:

let
  commonModule = { config, ... }: {
    users.users.check = {
      description = "Check User";
      hashedPassword = "";
      isNormalUser = true;
    };

    # The state version can safely track the latest release because the disk
    # image is ephermal.
    system.stateVersion = config.system.nixos.release;
    home-manager.users.check.home.stateVersion = config.system.nixos.release;

    virtualisation.vmVariant.virtualisation = {
      # This is a maximum limit; the VM should still work if the host has fewer cores.
      cores = 4;
      memorySize = 2048;
    };
  };

  autoload = builtins.concatLists
    (lib.mapAttrsToList
      (name: _:
        let check = {
          inherit name;
          module = "${../modules}/${name}/check.nix";
        };
        in
          lib.optional (builtins.pathExists check.module) check
      )
      (builtins.readDir ../modules));

  makeCheck =
    check: stylix:
    let
      name = "${check.name}-${stylix.polarity}";

      system = lib.nixosSystem {
        inherit (pkgs) system;
        modules = [
          inputs.self.nixosModules.stylix
          inputs.home-manager.nixosModules.home-manager
          commonModule
          check.module
          {
            inherit stylix;
            system.name = name;
          }
        ];
      };

      script = pkgs.writeShellApplication {
        inherit name;
        text = ''
          directory="''${XDG_CACHE_HOME:-$HOME/.cache}/stylix"
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

  makeChecks = check: map (makeCheck check) [
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
  lib.listToAttrs (lib.flatten (map makeChecks autoload))
