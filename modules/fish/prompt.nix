{ pkgs, config }:

let
  theme = config.stylix.colors {
    templateRepo = pkgs.fetchFromGitHub {
      owner = "tomyun";
      repo = "base16-fish";
      rev = "7f647967fddedaf803191bc9113b13d2071dc3cf";
      sha256 = "IGUbLjsmmAvB9UKGkR7oqdpjeVEfzt83GpyBkrZf2O4=";
    };
  };
in ''
  source ${theme}
  base16-${config.stylix.colors.slug}
''
