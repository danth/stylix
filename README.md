# Stylix

System-wide colorscheming and typography for [NixOS](https://nixos.org/),
built upon ideas from [Base16](http://chriskempson.com/projects/base16/).

## Installation

Stylix can be installed using the experimental
[flakes](https://nixos.wiki/wiki/Flakes) feature:

```nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    stylix.url = "github:danth/stylix";
  };

  outputs = { self, nixpkgs, home-manager, stylix }: {
    nixosConfigurations."<hostname>" = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        home-manager.nixosModules.home-manager
        stylix.nixosModules.stylix
      ];
    };
  };
}
```

Stylix relies on [Home Manager](https://github.com/nix-community/home-manager)
to install a lot of its theming. This requires Home Manager to be installed as
a NixOS module; how to do this is shown in the example above. Users must be
listed in `stylix.homeManagerUsers` to enable styles which rely on Home Manager
for that user.

## Configuration

```nix
{ pkgs, ... }:

{
  # A colorscheme will be chosen automatically based on your wallpaper
  stylix.image = ./wallpaper.png;

  # Select your preferred fonts, or use these defaults:
  stylix.fonts = {
    serif = {
      package = pkgs.dejavu_fonts;
      name = "DejaVu Serif";
    };
    sansSerif = {
      package = pkgs.dejavu_fonts;
      name = "DejaVu Sans";
    };
    monospace = {
      package = pkgs.dejavu_fonts;
      name = "DejaVu Sans Mono";
    };
    emoji = {
      package = pkgs.noto-fonts-emoji;
      name = "Noto Color Emoji";
    };
  };

  # Add users to this list to enable Home Manager integration
  stylix.homeManagerUsers = [ "danth" ];
}
```
