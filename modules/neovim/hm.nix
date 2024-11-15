{
  pkgs,
  config,
  lib,
  ...
}:

{
  options.stylix.targets.neovim = {
    enable = config.lib.stylix.mkEnableTarget "Neovim" true;
    plugin = lib.mkOption {
      type = lib.types.enum [
        "base16-nvim"
        "mini.base16"
      ];
      default = "mini.base16";
      description = "Plugin used for the colorscheme";
    };
    transparentBackground = {
      main = lib.mkEnableOption "background transparency for the main Neovim window";
      signColumn = lib.mkEnableOption "background transparency for the Neovim sign column";
    };
  };

  config =
    lib.mkIf (config.stylix.enable && config.stylix.targets.neovim.enable)
      {
        programs.neovim =
          let
            cfg = config.stylix.targets.neovim;
          in
          {
            plugins = [
              (lib.mkIf (cfg.plugin == "base16-nvim") {
                plugin = pkgs.vimPlugins.base16-nvim;
                type = "lua";
                config = with config.lib.stylix.colors.withHashtag; ''
                  require('base16-colorscheme').setup({
                    base00 = '${base00}', base01 = '${base01}', base02 = '${base02}', base03 = '${base03}',
                    base04 = '${base04}', base05 = '${base05}', base06 = '${base06}', base07 = '${base07}',
                    base08 = '${base08}', base09 = '${base09}', base0A = '${base0A}', base0B = '${base0B}',
                    base0C = '${base0C}', base0D = '${base0D}', base0E = '${base0E}', base0F = '${base0F}'
                  })
                '';
              })
              (lib.mkIf (cfg.plugin == "mini.base16") {
                plugin = pkgs.vimPlugins.mini-nvim;
                type = "lua";
                config = with config.lib.stylix.colors.withHashtag; ''
                  require('mini.base16').setup({
                    palette = {
                      base00 = '${base00}', base01 = '${base01}', base02 = '${base02}', base03 = '${base03}',
                      base04 = '${base04}', base05 = '${base05}', base06 = '${base06}', base07 = '${base07}',
                      base08 = '${base08}', base09 = '${base09}', base0A = '${base0A}', base0B = '${base0B}',
                      base0C = '${base0C}', base0D = '${base0D}', base0E = '${base0E}', base0F = '${base0F}'
                    }
                  })
                '';
              })
            ];

            extraLuaConfig = lib.mkMerge [
              (lib.mkIf cfg.transparentBackground.main ''
                vim.cmd.highlight({ "Normal", "guibg=NONE", "ctermbg=NONE" })
                vim.cmd.highlight({ "NonText", "guibg=NONE", "ctermbg=NONE" })
              '')
              (lib.mkIf cfg.transparentBackground.signColumn ''
                vim.cmd.highlight({ "SignColumn", "guibg=NONE", "ctermbg=NONE" })
              '')
            ];
          };
      };
}
