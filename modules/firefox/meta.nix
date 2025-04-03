{ lib, pkgs, ... }:
{
  maintainers = [ lib.maintainers.Flameopathic ];
  packages = with pkgs; [
    firefox
    floorp
    librewolf
  ];
}
