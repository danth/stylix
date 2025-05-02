{ pkgs, lib, ... }:
{
  stylix.testbed.ui.command.text = lib.getExe pkgs.eog;
}
