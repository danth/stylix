{ pkgs, inputs, lib, ... }:

let
  username = "guest";

  commonModule = { config, ... }: {
    users.users.${username} = {
      description = "Guest";
      hashedPassword = "";
      isNormalUser = true;
    };

    # The state version can safely track the latest release because the disk
    # image is ephermal.
    system.stateVersion = config.system.nixos.release;
    home-manager.users.${username}.home.stateVersion = config.system.nixos.release;

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
          commonModule
          inputs.self.nixosModules.stylix
          inputs.home-manager.nixosModules.home-manager
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
          # We create a temporary directory rather than a temporary file, since
          # temporary files are created empty and are not valid disk images.
          directory="$(mktemp --directory)"

          clean() {
            if rm --recursive "$directory"; then
              echo 'Virtualisation disk image removed.'
            fi
          }
          trap clean EXIT

          NIX_DISK_IMAGE="$directory/nixos.qcow2" \
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
