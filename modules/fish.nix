{ pkgs, config, ... }:

let
  base16-fish = pkgs.fetchFromGitHub {
    owner = "tomyun";
    repo = "base16-fish";
    rev = "7f647967fddedaf803191bc9113b13d2071dc3cf";
    sha256 = "IGUbLjsmmAvB9UKGkR7oqdpjeVEfzt83GpyBkrZf2O4=";
  };

  theme = config.lib.stylix.base16.buildTemplate "fish"
    "${base16-fish}/templates/default.mustache";

  promptInit = ''
    source ${theme}
    base16-stylix
  '';

in {
  programs.fish.promptInit = promptInit;

  stylix.homeModule = { programs.fish.promptInit = promptInit; };
}
