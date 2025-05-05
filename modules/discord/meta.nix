{ lib, ... }:
{
  name = "Discord";
  homepage = {
    Discord = "https://discordapp.com/";
    Nixcord = "https://github.com/KaylorBen/nixcord";
    Vencord = "https://github.com/Vendicated/Vencord";
    Vesktop = "https://github.com/Vencord/Vesktop";
  };
  maintainers = [ lib.maintainers.Flameopathic ];
  description = ''
    This module provides a collection of targets related to
    [Discord](https://discord.com). The same theme is used for each target.

    The Vencord, Vesktop, and Nixcord targets use built-in Home Manager options,
    while the Nixcord target leverages
    [github:KaylorBen/nixcord](https://github.com/KaylorBen/nixcord).

    > [!IMPORTANT]
    > The Nixcord target will have no effect unless Nixcord is properly
    > [imported](https://github.com/KaylorBen/nixcord?tab=readme-ov-file#how-to-use-nixcord)
    > into your configuration.
  '';
}
