{ pkgs, config, ... }:

let
  themeFile = config.lib.stylix.colors {
    templateRepo = pkgs.fetchFromGitHub {
      owner = "chriskempson";
      repo = "base16-vim";
      rev = "6191622d5806d4448fa2285047936bdcee57a098";
      sha256 = "6FsT87qcl9GBxgxrPx2bPULIMA/O8TRxHaN49qMM4uM=";
    };
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

  vimOptions = {
    plugins = [ themePlugin ];
    extraConfig = "colorscheme base16-stylix";
  };

in {
  home-manager.sharedModules = [{
    programs.vim = vimOptions;
    programs.neovim = vimOptions;
  }];
}
