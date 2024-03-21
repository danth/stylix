{ pkgs, config, lib, ... }:

{
  options.stylix.targets.nixos.enable =
    config.lib.stylix.mkEnableTarget "the NixOS logo" true;

  config.nixpkgs.overlays = lib.mkIf config.stylix.targets.nixos.enable [(self: super: {
    nixos-icons = super.nixos-icons.overrideAttrs (oldAttrs: {
      patchPhase = ''
        substituteInPlace logo/white.svg \
          --replace-fail '#ffffff' '#${config.lib.stylix.colors.base05}'
        ${lib.getExe pkgs.resvg} logo/white.svg logo/white.png
      '';
    });
  })];
}