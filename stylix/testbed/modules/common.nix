{ lib, config, ... }:
let
  user = lib.importTOML ../user.toml;
in
{
  users.users.${user.username} = builtins.removeAttrs user [ "username" ];

  security.sudo.wheelNeedsPassword = false;

  services.getty.autologinUser = user.username;

  nixpkgs.config.allowAliases = false;

  # The state version can safely track the latest release because the disk
  # image is ephemeral.
  system.stateVersion = config.system.nixos.release;
  home-manager.users.${user.username}.home.stateVersion =
    config.system.nixos.release;

  virtualisation.vmVariant.virtualisation = {
    # This is a maximum limit; the VM should still work if the host has fewer cores.
    cores = 4;
    memorySize = lib.mkDefault 2048;
  };
}
