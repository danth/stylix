{
  lib,
  config,
  pkgs,
  ...
}:
let
  user = lib.importTOML ../user.toml;
in
{
  options.stylix.testbed.ui = lib.mkOption {
    type = lib.types.nullOr (
      lib.types.submodule {
        options = {
          enable = lib.mkOption {
            type = lib.types.bool;
            default = false;
            description = ''
              Whether to enable a standard configuration for testing graphical
              applications.

              This will automatically log in as the `${user.username}` user and launch
              an application or command.

              This is currently based on GNOME, but the specific desktop environment
              used may change in the future.
            '';
          };
          application = lib.mkOption {
            description = ''
              Options defining an application to be launched using its provided
              `.desktop` entry.
            '';
            type = lib.types.nullOr (
              lib.types.submodule {
                options = {
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
                      The package providing the binary and desktop entry of the
                      application being tested.
                    '';
                  };
                };
              }
            );
            default = null;
          };
          command = lib.mkOption {
            type = lib.types.nullOr (
              lib.types.submodule {
                options = {
                  text = lib.mkOption {
                    type = lib.types.str;
                    description = ''
                      The command which will be run once the graphical environment has
                      loaded.
                    '';
                  };
                  useTerminal = lib.mkOption {
                    type = lib.types.bool;
                    description = ''
                      Whether or not to spawn a terminal when running the command.
                    '';
                    default = false;
                  };
                };
              }
            );
            default = null;
          };
        };
      }
    );
    default = null;
  };

  config = lib.mkIf (config.stylix.testbed.ui != null) {
    services.xserver = {
      enable = true;
      displayManager.gdm.enable = true;
      desktopManager.gnome.enable = true;
    };

    services.displayManager.autoLogin = {
      enable = true;
      user = user.username;
    };

    # Disable the GNOME tutorial which pops up on first login.
    environment.gnome.excludePackages = [ pkgs.gnome-tour ];

    # for use when application is set
    environment.systemPackages =
      lib.optional (config.stylix.testbed.ui.command != null) (
        pkgs.makeAutostartItem {
          name = "stylix-testbed";
          package = pkgs.makeDesktopItem {
            name = "stylix-testbed";
            desktopName = "stylix-testbed";
            exec = toString (
              pkgs.writeShellScript "startup" config.stylix.testbed.ui.command.text
            );
            terminal = config.stylix.testbed.ui.command.useTerminal;
          };
        }
      )
      ++ lib.optional (config.stylix.testbed.ui.application != null) (
        pkgs.makeAutostartItem {
          inherit (config.stylix.testbed.ui.application) name package;
        }
      );
  };
}
