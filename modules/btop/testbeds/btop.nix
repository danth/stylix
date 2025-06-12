{ lib, pkgs, ... }:
{
  stylix.testbed.ui.command = {
    text = lib.getExe pkgs.btop;
    useTerminal = true;
  };

  home-manager.sharedModules = lib.singleton {
    programs.btop.enable = true;
  };
}
