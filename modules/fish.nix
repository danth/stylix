{ pkgs, config, ... }:

let
  theme = config.lib.stylix.colors {
    templateRepo = pkgs.fetchFromGitHub {
      owner = "tomyun";
      repo = "base16-fish";
      rev = "7f647967fddedaf803191bc9113b13d2071dc3cf";
      sha256 = "IGUbLjsmmAvB9UKGkR7oqdpjeVEfzt83GpyBkrZf2O4=";
    };
  };

  promptInit = ''
    source ${theme}
    base16-stylix
  '';

in {
  programs.fish.promptInit = promptInit;

  stylix.homeModule = { programs.fish.interactiveShellInit = promptInit; };
}
