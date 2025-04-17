{ lib, ... }:
{
  name = "mpv";
  homepages = "https://mpv.io/";
  maintainers = with lib.maintainers; [
    awwpotato
    naho
  ];
}
