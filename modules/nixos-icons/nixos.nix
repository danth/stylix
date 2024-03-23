{ pkgs, config, lib, ... }:

with config.lib.stylix.colors;

{
  options.stylix.targets.nixos-icons.enable =
    config.lib.stylix.mkEnableTarget "the NixOS logo" true;

  config.nixpkgs.overlays = lib.mkIf config.stylix.targets.nixos-icons.enable [(self: super: {
    nixos-icons = super.nixos-icons.overrideAttrs (oldAttrs: {
      patchPhase = ''
        substituteInPlace logo/white.svg --replace-fail '#ffffff' '#${base05}'

        # Insert attribution comment after the XML prolog
        sed \
          --in-place \
          '2i<!-- The original NixOS logo from ${oldAttrs.src.url} is licensed under https://creativecommons.org/licenses/by/4.0 and has been modified to match the ${scheme} color scheme. -->' \
          logo/white.svg

        ${lib.getExe pkgs.resvg} logo/white.svg logo/white.png
      '';
    });
  })];
}
