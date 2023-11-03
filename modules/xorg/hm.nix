{pkgs, config, lib, ... }@args:

let command = import ./command.nix args;

in {
  options.stylix.targets.xorg.enable =
    config.lib.stylix.mkEnableTarget
      "the desktop background for X window managers"
      (with config.xsession.windowManager;
        bspwm.enable
        || herbstluftwm.enable
        || i3.enable
        || spectrwm.enable
        || xmonad.enable);

  config.xsession = lib.mkIf config.stylix.targets.xorg.enable {
    enable = true;
    initExtra = command;
  };
}
