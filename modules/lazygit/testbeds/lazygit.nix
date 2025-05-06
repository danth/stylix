{ lib, pkgs, ... }:

{
  stylix.testbed.ui.command = {
    text = ''
      ${lib.getExe pkgs.git} init
      ${lib.getExe pkgs.lazygit}
    '';
    useTerminal = true;
  };

  home-manager.sharedModules = lib.singleton {
    programs = {
      git.enable = true;
      lazygit.enable = true;
    };
  };
}
