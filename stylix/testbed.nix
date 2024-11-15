{
  pkgs,
  inputs,
  lib,
  ...
}:

let
  username = "guest";

  commonModule =
    { config, ... }:
    {
      users.users.${username} = {
        description = "Guest";
        hashedPassword = "";
        isNormalUser = true;
        extraGroups = [ "wheel" ];
      };

      security.sudo.wheelNeedsPassword = false;

      # The state version can safely track the latest release because the disk
      # image is ephemeral.
      system.stateVersion = config.system.nixos.release;
      home-manager.users.${username}.home.stateVersion = config.system.nixos.release;

      virtualisation.vmVariant.virtualisation = {
        # This is a maximum limit; the VM should still work if the host has fewer cores.
        cores = 4;
        memorySize = lib.mkDefault 2048;
      };
    };

  applicationModule =
    { config, lib, ... }:
    {
      options.stylix.testbed.application = {
        enable = lib.mkOption {
          type = lib.types.bool;
          default = false;
          description = ''
            Whether to enable a standard configuration for testing individual
            applications.

            This will automatically log in as the `${username}` user, then launch
            the application from its desktop entry.

            This is currently based on GNOME, but the specific desktop environment
            used may change in the future.
          '';
        };

        name = lib.mkOption {
          type = lib.types.str;
          description = ''
            The name of the desktop entry for the application, without the
            `.desktop` extension.
          '';
        };

        package = lib.mkOption {
          type = lib.types.package;
          description = ''
            The application being tested.
          '';
        };
      };

      config = lib.mkIf config.stylix.testbed.application.enable {
        services.xserver = {
          enable = true;
          displayManager.gdm.enable = true;
          desktopManager.gnome.enable = true;
        };

        services.displayManager.autoLogin = {
          enable = true;
          user = username;
        };

        # Disable the GNOME tutorial which pops up on first login.
        environment.gnome.excludePackages = [ pkgs.gnome-tour ];

        environment.systemPackages = [
          (pkgs.makeAutostartItem {
            inherit (config.stylix.testbed.application) name package;
          })
        ];
      };
    };

  autoload = builtins.concatLists (
    lib.mapAttrsToList (
      name: _:
      let
        testbed = {
          inherit name;
          module = "${inputs.self}/modules/${name}/testbed.nix";
        };
      in
      lib.optional (builtins.pathExists testbed.module) testbed
    ) (builtins.readDir "${inputs.self}/modules")
  );

  makeTestbed =
    testbed: stylix:
    let
      name = "testbed-${testbed.name}-${stylix.polarity}";

      system = lib.nixosSystem {
        inherit (pkgs) system;

        modules = [
          commonModule
          applicationModule
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
          cleanup() {
            if rm --recursive "$directory"; then
              printf '%s\n' 'Virtualisation disk image removed.'
            fi
          }

          # We create a temporary directory rather than a temporary file, since
          # temporary files are created empty and are not valid disk images.
          directory="$(mktemp --directory)"
          trap cleanup EXIT

          NIX_DISK_IMAGE="$directory/nixos.qcow2" \
            ${lib.getExe system.config.system.build.vm}
        '';
      };
    in
    lib.nameValuePair name script;

  # This generates a copy of each testbed for each of the following themes.
  makeTestbeds =
    testbed:
    map (makeTestbed testbed) [
      {
        enable = true;
        image = pkgs.fetchurl {
          name = "three-bicycles.jpg";
          url = "https://unsplash.com/photos/hwLAI5lRhdM/download?ixid=M3wxMjA3fDB8MXxhbGx8fHx8fHx8fHwxNzE2MzYxNDcwfA&force=true";
          hash = "sha256-S0MumuBGJulUekoGI2oZfUa/50Jw0ZzkqDDu1nRkFUA=";
        };
        base16Scheme = "${pkgs.base16-schemes}/share/themes/catppuccin-latte.yaml";
        polarity = "light";
      }
      {
        enable = true;
        image = pkgs.fetchurl {
          name = "mountains.jpg";
          url = "https://unsplash.com/photos/ZqLeQDjY6fY/download?ixid=M3wxMjA3fDB8MXxhbGx8fHx8fHx8fHwxNzE2MzY1NDY4fA&force=true";
          hash = "sha256-Dm/0nKiTFOzNtSiARnVg7zM0J1o+EuIdUQ3OAuasM58=";
        };
        base16Scheme = "${pkgs.base16-schemes}/share/themes/catppuccin-macchiato.yaml";
        polarity = "dark";
      }
    ];

in
lib.listToAttrs (lib.flatten (map makeTestbeds autoload))
