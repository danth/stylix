{ lib, ... }:
{
  name = "Firefox and its derivatives";
  homepages = {
    Firefox = "http://www.mozilla.com/en-US/firefox/";
    Floorp = "https://floorp.app/";
    LibreWolf = "https://librewolf.net/";
  };
  maintainers = with lib.maintainers; [
    Flameopathic
    danth
  ];
}
