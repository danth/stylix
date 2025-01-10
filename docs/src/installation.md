# Installation

## NixOS

You can install Stylix into your NixOS configuration using [Flakes][nix-flakes].
This will provide theming for system level programs such as bootloaders, splash
screens, and display managers.

```nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    stylix.url = "github:danth/stylix";
  };

  outputs = { nixpkgs, stylix, ... }: {
    nixosConfigurations."«hostname»" = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [ stylix.nixosModules.stylix ./configuration.nix ];
    };
  };
}
```
<small>Minimal `flake.nix` for a NixOS configuration.</small>

If you are using a stable release of NixOS, ensure that you use the matching
Stylix release. For example:

```nix
{
  nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
  stylix.url = "github:danth/stylix/release-24.11";
}
```

Many applications cannot be configured system wide, so Stylix will also need
[Home Manager][nix-hm] to be able to change their settings within your home
directory.

[Installing Home Manager as a NixOS module](https://nix-community.github.io/home-manager/index.xhtml#sec-install-nixos-module)
is highly recommended if you don't use it already. This will combine it with
your existing configuration, so you don't need to run any extra commands when
you rebuild, and the theme you set in NixOS will automatically be used for Home
Manager too.

When Stylix is installed to a NixOS configuration, it will automatically set up
its Home Manager modules if it detects that Home Manager is available. You can
theoretically use it without installing Home Manager, however most features
will be unavailable.

## nix-darwin

You can install Stylix into your nix-darwin configuration in a similar fashion
to NixOS via [Flakes][nix-flakes].

```nix
{
  inputs = {
    darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    stylix.url = "github:danth/stylix";
  };

  outputs = { darwin, nixpkgs, stylix, ... }: {
    darwinConfigurations."«hostname»" = darwin.lib.darwinSystem {
      system = "aarch64-darwin";
      modules = [ stylix.darwinModules.stylix ./configuration.nix ];
    };
  };
}
```
<small>Minimal `flake.nix` for a nix-darwin configuration.</small>

While this won't have an effect on the looks of MacOS, since we don't have the
controls to theme it like we do NixOS, it will automatically set up the [Home
Manager][nix-hm] modules for you.

## Home Manager

If you would prefer to use the standalone version of Home Manager, you can
install Stylix directly into your Home Manager configuration instead. This
could be useful if you are on another operating system, or a machine which
is managed by someone else.


```nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    stylix.url = "github:danth/stylix";
  };

  outputs = { nixpkgs, home-manager, stylix, ... }: {
    homeConfigurations."«username»" = home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      modules = [ stylix.homeManagerModules.stylix ./home.nix ];
    };
  };
}
```
<small>Minimal `flake.nix` for a Home Manager configuration.</small>

If you are using a stable release of Home Manager, ensure that you use the
matching Stylix release. For example:

```nix
{
  nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
  home-manager.url = "github:nix-community/home-manager/release-24.11";
  stylix.url = "github:danth/stylix/release-24.11";
}
```

If you choose to use both NixOS and Home Manager but configure them separately,
you will need to copy your theme into both of your configurations, as keeping them
separate means they cannot follow each other automatically.

## Without flakes

If you haven't enabled flakes yet or don't want to use this feature, `default.nix`
re-exports all the flake outputs, without requiring flakes to be enabled. This means
that once you have a copy of this repo, using either a local checkout,
[niv](https://github.com/nmattia/niv), or any other method, you can import it to
get the NixOS module as the `nixosModules.stylix` attribute and the Home Manager
module as the `homeManagerModules.stylix` attribute.

```nix
let
  stylix = pkgs.fetchFromGitHub {
      owner = "danth";
      repo = "stylix";
      rev = "...";
      sha256 = "...";
  };
in {
    imports = [ (import stylix).homeManagerModules.stylix ];

    stylix = {
      enable = true;
      image = ./wallpaper.jpg;
    };
}

```
<small>Example usage of the Home Manager module without flakes.</small>

[nix-flakes]: https://wiki.nixos.org/wiki/Flakes
[nix-hm]: https://github.com/nix-community/home-manager
