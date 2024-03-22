{ pkgs, config, lib, ... }:

with config.lib.stylix.colors;

{
  options.stylix.targets.nixos.enable =
    config.lib.stylix.mkEnableTarget "the NixOS logo" true;

  config.nixpkgs.overlays = lib.mkIf config.stylix.targets.nixos.enable [(self: super: {
    nixos-icons = super.nixos-icons.overrideAttrs (oldAttrs: {
      patchPhase = ''
        substituteInPlace logo/white.svg --replace-fail '#ffffff' '#${base05}'
        sed -i '2i<!-- The original NixOS logo from ${oldAttrs.src.url} modified to use colors from ${scheme}. This file is licensed under https://creativecommons.org/licenses/by/4.0 -->' logo/white.svg
        ${lib.getExe pkgs.resvg} logo/white.svg logo/white.png
      '';
    });
  })];
}
