{ lib, ... }:
{
  name = "zathura";
  homepages = "https://pwmt.org/projects/zathura/";
  maintainers = with lib.maintainers; [
    mateusauler
    naho
  ];
}
