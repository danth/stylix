{
  config,
  lib,
  options,
  ...
}:
let
  cfg = config.stylix.targets.nvf;
in
{
  options.stylix.targets.nvf = {
    enable = config.lib.stylix.mkEnableTarget "nvf" true;
    plugin = lib.mkOption {
      type = lib.types.enum [
        "base16"
        "mini-base16"
      ];
      default = "base16";
      description = "Plugin used for the colorscheme";
    };
    transparentBackground = lib.mkEnableOption "background transparency for the main Neovim window";
  };

  config =
    lib.mkIf (config.stylix.enable && cfg.enable && options.programs ? nvf)
      (
        lib.optionalAttrs (options.programs ? nvf) {
          programs.nvf.settings.vim = {
            theme = {
              enable = true;
              name = cfg.plugin;
              base16-colors = {
                inherit (config.lib.stylix.colors.withHashtag)
                  base00
                  base01
                  base02
                  base03
                  base04
                  base05
                  base06
                  base07
                  base08
                  base09
                  base0A
                  base0B
                  base0C
                  base0D
                  base0E
                  base0F
                  ;
              };
              transparent = cfg.transparentBackground;
            };
            statusline = lib.mkIf (cfg.plugin == "base16") {
              lualine.theme = "base16";
            };
          };
        }
      );
}
