{
  config,
  lib,
  options,
  ...
}: {
  options.stylix.targets.nixvim.enable =
    config.lib.stylix.mkEnableTarget "nixvim" (config.programs ? nixvim);

  config = lib.mkIf ((config.programs ? nixvim) && config.stylix.targets.nixvim.enable) (
    lib.optionalAttrs (builtins.hasAttr "nixvim" options.programs) {
      programs.nixvim.colorschemes.base16.customColorScheme = let
        colors = config.lib.stylix.colors.withHashtag;
      in {
        base00 = colors.base00;
        base01 = colors.base01;
        base02 = colors.base02;
        base03 = colors.base03;
        base04 = colors.base04;
        base05 = colors.base05;
        base06 = colors.base06;
        base07 = colors.base07;
        base08 = colors.base08;
        base09 = colors.base09;
        base0A = colors.base0A;
        base0B = colors.base0B;
        base0C = colors.base0C;
        base0D = colors.base0D;
        base0E = colors.base0E;
        base0F = colors.base0F;
      };
      programs.nixvim.colorschemes.base16.enable = true;
    }
  );
}
