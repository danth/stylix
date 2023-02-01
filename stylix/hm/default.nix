{ palette-generator, base16 }:
{ config, lib, ... }@mod-args:

let
  inherit (lib) types;
  from-nixos = mod-args ? "osConfig";
  system-theme = config.stylix.useSystemTheme;
  autoload = import ../autoload.nix { inherit lib; } "hm";
in {
  imports = [
    ../pixel.nix
    ../target.nix
    ./fonts.nix
    (import ./palette.nix { inherit palette-generator base16; })
  ] ++ autoload;

  options.stylix = {
    useSystemTheme = lib.mkOption {
      type = types.bool;
      description = ''
        Use NixOS stylix theme for this user.

        Doesn't do anything if the home-manager configuration is not used
        from NixOS.
      '';
      default =
        if from-nixos
          then mod-args.osConfig.stylix.homeManagerIntegration.enable
          else false;
    };
  };

  config = lib.mkMerge [
    (lib.mkIf (system-theme && !from-nixos) {
      warnings = [
        "stylix.useSystemTheme is ignored if the config is not used from NixOS"
      ];
    })
    (lib.mkIf (system-theme && from-nixos) (let
      cfg = mod-args.osConfig.stylix;
      mk = lib.mkOverride 99;
    in {
      stylix.autoEnable = mk cfg.autoEnable;
      stylix.fonts.serif = mk cfg.fonts.serif;
      stylix.fonts.sansSerif = mk cfg.fonts.sansSerif;
      stylix.fonts.monospace = mk cfg.fonts.monospace;
      stylix.fonts.emoji = mk cfg.fonts.emoji;
      stylix.polarity = mk cfg.polarity;
      stylix.image = mk cfg.image;
      stylix.palette.base00 = cfg.palette.base00;
      stylix.palette.base01 = cfg.palette.base01;
      stylix.palette.base02 = cfg.palette.base02;
      stylix.palette.base03 = cfg.palette.base03;
      stylix.palette.base04 = cfg.palette.base04;
      stylix.palette.base05 = cfg.palette.base05;
      stylix.palette.base06 = cfg.palette.base06;
      stylix.palette.base07 = cfg.palette.base07;
      stylix.palette.base08 = cfg.palette.base08;
      stylix.palette.base09 = cfg.palette.base09;
      stylix.palette.base0A = cfg.palette.base0A;
      stylix.palette.base0B = cfg.palette.base0B;
      stylix.palette.base0C = cfg.palette.base0C;
      stylix.palette.base0D = cfg.palette.base0D;
      stylix.palette.base0E = cfg.palette.base0E;
      stylix.palette.base0F = cfg.palette.base0F;
      stylix.base16Scheme = cfg.base16Scheme;
    }))
  ];
}
