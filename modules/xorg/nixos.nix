{pkgs, config, lib, ... }@args:

let command = import ./command.nix args;

in {
  options.stylix.targets.xorg.enable =
    config.lib.stylix.mkEnableTarget
      "the desktop background for X window managers"
      (with config.services.xserver.windowManager;
        xmonad.enable || i3.enable);

  config.services.xserver.displayManager.sessionCommands =
    lib.mkIf config.stylix.targets.xorg.enable command;
}
