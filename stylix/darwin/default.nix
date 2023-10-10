inputs:
{ palette-generator, base16, homeManagerModule }:
{ options, config, lib, ... }:

let
  hm = config.stylix.homeManagerIntegration;
  autoload = import ../autoload.nix { inherit lib; } "darwin";
in {
  imports = [
    ../pixel.nix
    ../target.nix
    ./fonts.nix
    (import ./palette.nix { inherit palette-generator base16; })
    (import ../templates.nix inputs)
  ] ++ autoload;

  options.stylix.homeManagerIntegration = {
    followSystem = lib.mkOption {
      description = lib.mdDoc ''
        When this option is `true`, Home Manager will follow
        the system theme by default, rather than requiring a theme to be set.

        This will only affect Home Manager configurations which are built
        within the nix-darwin configuration.
      '';
      type = lib.types.bool;
      default = true;
    };

    autoImport = lib.mkOption {
      description = lib.mdDoc ''
        Whether to enable Stylix automatically for every user.

        This only applies to users for which Home Manager is set up within the
        nix-darwin configuration.
      '';
      type = lib.types.bool;
      default = options ? home-manager;
      defaultText = lib.literalMD ''
        `true` when Home Manager is present.
      '';
    };
  };

  config = lib.mkIf hm.autoImport {
    home-manager.sharedModules = [ homeManagerModule ];
  };
}
