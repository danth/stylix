{ pkgs, config, ... }:

{
  stylix.image = "${pkgs.pantheon.elementary-wallpapers}/share/backgrounds/Ashim DSilva.jpg";

  users.users.login = {
    description = "User Account";
    hashedPassword = "";
    isNormalUser = true;
  };

  system.stateVersion = config.system.nixos.release;
  home-manager.users.login.home.stateVersion = config.system.nixos.release;

  virtualisation.vmVariant.virtualisation = {
    # Allocate more resources so that desktop environments run better.
    cores = 2;
    memorySize = 2048;

    # By default the disk image is saved to the current directory, move it to
    # /run to avoid it being committed by accident. Delete the image each time
    # the VM is started to ensure that no state is left behind.
    diskImage =
      let path = "/run/user/$UID/stylix/${config.system.name}.qcow2";
      in "$(mkdir -p $(dirname ${path}) && rm -f ${path} && echo ${path})";

    # Limit the disk image to 128MiB, as /run is usually held in memory.
    diskSize = 128;
  };
}
