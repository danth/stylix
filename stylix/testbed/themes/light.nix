{ pkgs, config, ... }:
let
  images = pkgs.callPackages ../images.nix { };
  inherit (config.stylix.inputs) tinted-schemes;
in
{
  stylix = {
    enable = true;
    image = images.light;
    base16Scheme = "${tinted-schemes}/base16/catppuccin-latte.yaml";
    polarity = "light";
    cursor = {
      name = "Vanilla-DMZ";
      package = pkgs.vanilla-dmz;
      size = 32;
    };
  };
}
