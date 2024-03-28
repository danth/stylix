{ pkgs, config, lib, ... }:

with lib;

let
  themeFile = config.lib.stylix.colors {
    templateRepo = config.lib.stylix.templates.base16-vim;
  };

  themePlugin = pkgs.vimUtils.buildVimPlugin {
    name = "stylix";
    pname = "stylix";

    src = themeFile;
    dontUnpack = true;

    buildPhase = ''
      install -D $src $out/colors/base16-stylix.vim
    '';
  };

  vimOptions = let
    inherit (config.stylix) fonts;
    inherit (fonts) sizes;
    monospace = builtins.head fonts.monospace;
  in {
    plugins = [ themePlugin ];
    extraConfig = ''
      set termguicolors
      colorscheme base16-stylix
      unlet g:colors_name
      set guifont=${escape [" "] monospace.name}:h${toString sizes.terminal}
    '';
  };

in {
  options.stylix.targets.vim.enable =
    config.lib.stylix.mkEnableTarget "Vim and/or Neovim" true;

  config = lib.mkIf config.stylix.targets.vim.enable {
    programs.vim = vimOptions;
    programs.neovim = vimOptions;
  };
}
