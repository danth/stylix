{ lib, ... }:
{
  name = "Discord";
  homepages = {
    Discord = "https://discordapp.com/";
    Vencord = "https://github.com/Vendicated/Vencord";
    Vesktop = "https://github.com/Vencord/Vesktop";
    Nixcord = "https://github.com/KaylorBen/nixcord";
  };
  maintainers = [ lib.maintainers.Flameopathic ];
}
