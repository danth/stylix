{ lib, ... }:
{
  maintainers = with lib.maintainers; [
    mateusauler
    naho
  ];

  name = "zathura";
}
