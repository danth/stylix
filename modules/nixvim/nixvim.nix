{
  config,
  lib,
  ...
}: {
  options.stylix.targets.nixvim.enable =
    config.lib.stylix.mkEnableTarget "nixvim" true;

  config = lib.mkIf config.stylix.targets.nixvim.enable {
    programs.nixvim.colorschemes.base16.customColorScheme = let
      colors = config.lib.stylix.colors;
      prependHash = color: "#${color}";
    in {
      base00 = prependHash colors.base00;
      base01 = prependHash colors.base01;
      base02 = prependHash colors.base02;
      base03 = prependHash colors.base03;
      base04 = prependHash colors.base04;
      base05 = prependHash colors.base05;
      base06 = prependHash colors.base06;
      base07 = prependHash colors.base07;
      base08 = prependHash colors.base08;
      base09 = prependHash colors.base09;
      base0A = prependHash colors.base0A;
      base0B = prependHash colors.base0B;
      base0C = prependHash colors.base0C;
      base0D = prependHash colors.base0D;
      base0E = prependHash colors.base0E;
      base0F = prependHash colors.base0F;
    };
  };
}
