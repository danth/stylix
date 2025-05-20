{ lib, ... }:
{
  name = "zathura";
  homepage = "https://pwmt.org/projects/zathura/";
  maintainers = with lib.maintainers; [
    mateusauler
    naho
  ];
}
