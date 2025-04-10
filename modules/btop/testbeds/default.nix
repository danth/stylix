{ lib, pkgs, ... }:
{
  stylix.testbed.ui.command = {
    text = lib.getExe pkgs.btop;
    useTerminal = true;
  };
}
