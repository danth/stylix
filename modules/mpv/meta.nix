{ lib, ... }:
{
  name = "mpv";
  homepage = "https://mpv.io/";
  maintainers = with lib.maintainers; [
    awwpotato
    naho
  ];
}
