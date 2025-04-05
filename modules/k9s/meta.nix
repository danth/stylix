{ lib, pkgs, ... }:
{
  maintainers = [ lib.maintainers.cluther ];
  packages = [ pkgs.k9s ];
}
