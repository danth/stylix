{ pkgs, config, ... }:
let
  images = pkgs.callPackages ../images.nix { };
  inherit (config.stylix.inputs) tinted-schemes;
in
{
  stylix = {
    enable = true;
    image = images.dark;
    base16Scheme = "${tinted-schemes}/base16/catppuccin-macchiato.yaml";
    polarity = "dark";
    cursor = {
      name = "Vanilla-DMZ";
      package = pkgs.vanilla-dmz;
      size = 32;
    };
  };
}
