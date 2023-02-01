{ palette-generator, base16, homeManagerModule }:
{ options, config, lib, ... }:

let
  hm = config.stylix.homeManagerIntegration;
  autoload = import ../autoload.nix { inherit lib; } "nixos";
in {
  imports = [
    ../pixel.nix
    ../target.nix
    ./fonts.nix
    (import ./palette.nix { inherit palette-generator base16; })
  ] ++ autoload;

  options.stylix.homeManagerIntegration = {
    enable = lib.mkOption {
      description = ''
        Enable home-manager integration

        This means that by default all the users will use the system
        theme, and the stylix hm module will be included in all users
        configurations.
      '';
      type = lib.types.bool;
      default = options ? home-manager;
      defaultText = "true if home-manager is imported";
    };

    disableImport = lib.mkOption {
      description = ''
        When the home-manager integration is enabled, do not automatically
        imports stylix into the user configuration.
      '';
      type = lib.types.bool;
      default = false;
    };
  };

  config = lib.mkIf (hm.enable && !hm.disableImport) {
    home-manager.sharedModules = [
      homeManagerModule
    ];
  };
}
