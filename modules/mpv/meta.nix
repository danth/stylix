{ lib, ... }:
{
  maintainers = with lib.maintainers; [
    awwpotato
    naho
  ];

  name = "mpv";
}
