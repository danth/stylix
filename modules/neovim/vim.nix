mkTarget:
{
  lib,
  pkgs,
  ...
}:
mkTarget {
  name = "vim";
  humanName = "Vim";

  configElements = [
    (
      { fonts }:
      {
        programs.vim.extraConfig = ''
          set guifont=${
            lib.escape [ " " ] fonts.monospace.name
          }:h${toString fonts.sizes.terminal}
        '';
      }
    )
    (
      { colors, inputs }:
      {
        programs.vim = {
          plugins = [
            (pkgs.vimUtils.buildVimPlugin {
              name = "stylix";
              pname = "stylix";

              src = colors {
                templateRepo = inputs.base16-vim;
                target = "base16";
              };
              dontUnpack = true;

              buildPhase = ''
                install -D $src $out/colors/base16-stylix.vim
              '';
            })
          ];
          extraConfig = with colors.withHashtag; ''
            set termguicolors
            colorscheme base16-stylix
            unlet g:colors_name

            let g:stylix_colors = {
              \ 'base00': '${base00}',
              \ 'base01': '${base01}',
              \ 'base02': '${base02}',
              \ 'base03': '${base03}',
              \ 'base04': '${base04}',
              \ 'base05': '${base05}',
              \ 'base06': '${base06}',
              \ 'base07': '${base07}',
              \ 'base08': '${base08}',
              \ 'base09': '${base09}',
              \ 'base0A': '${base0A}',
              \ 'base0B': '${base0B}',
              \ 'base0C': '${base0C}',
              \ 'base0D': '${base0D}',
              \ 'base0E': '${base0E}',
              \ 'base0F': '${base0F}',
            \ }
          '';
        };
      }
    )
  ];
}
