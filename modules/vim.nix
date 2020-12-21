{ pkgs, config, ... }:

let
  base16-vim = pkgs.fetchFromGitHub {
    owner = "chriskempson";
    repo = "base16-vim";
    rev = "6191622d5806d4448fa2285047936bdcee57a098";
    sha256 = "6FsT87qcl9GBxgxrPx2bPULIMA/O8TRxHaN49qMM4uM=";
  };

  themePlugin = pkgs.vimUtils.buildVimPlugin {
    name = "stylix";

    src = base16-vim;
    data = config.lib.stylix.base16.json;
    passAsFile = [ "data" ];

    buildPhase = ''
      # Remove pre-built color schemes
      rm colors/*

      ${pkgs.mustache-go}/bin/mustache $dataPath \
        templates/default.mustache > colors/base16-stylix.vim
    '';
  };

  vimOptions = {
    plugins = [ themePlugin ];
    extraConfig = "colorscheme base16-stylix";
  };

in {
  stylix.homeModule = {
    programs.vim = vimOptions;
    programs.neovim = vimOptions;
  };
}
