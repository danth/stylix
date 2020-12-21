# Stylix

System-wide colorscheming and typography for [NixOS](https://nixos.org/),
built upon ideas from [Base16](http://chriskempson.com/projects/base16/).

## Installation

Import `default.nix` into your system configuration:

```nix
{ pkgs, ... }:

let stylix = pkgs.fetchFromGitHub {
  owner = "danth";
  repo = "stylix";
  rev = "...";
  sha256 = "...";
};

in {
  imports = [ "${stylix}/default.nix" ];
}
```

### Home Manager

Stylix relies on [Home Manager](https://github.com/nix-community/home-manager)
to install a lot of its theming. This requires Home Manager to be installed as
a NixOS module, if you do not already have that set up you will need to follow
[these instructions](https://rycee.gitlab.io/home-manager/index.html#sec-install-nixos-module).

### Nix Flakes

Stylix can also be installed using the experimental
[flakes](https://nixos.wiki/wiki/Flakes) feature:

```nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    stylix.url = "github:danth/stylix";
  };

  outputs = { self, nixpkgs, stylix }: {
    nixosConfigurations."<hostname>" = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [ stylix.nixosModules.stylix ];
    };
  };
}
```

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
