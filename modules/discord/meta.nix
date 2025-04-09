{ lib, pkgs, ... }:
{
  maintainers = [ lib.maintainers.Flameopathic ];
  packages = with pkgs; [
    discord
    vencord
    vesktop
  ];
  homepages.nixcord = "https://github.com/KaylorBen/nixcord";
}
