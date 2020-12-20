{ pkgs, config, ... }:

let
  # When https://github.com/tomyun/base16-fish/pull/5 is merged,
  # this can use the main repository rather than a fork
  base16-fish = pkgs.fetchFromGitHub {
    owner = "ngmoviedo";
    repo = "base16-fish";
    rev = "2aa139c901c8568764dbdc13934862178cb84595";
    sha256 = "ppYxLr+wD42Xwsq8dk6MLk8TrudTnlQlgvFf4ZyTjVk=";
  };

  theme = config.lib.stylix.base16.buildTemplate "fish"
    "${base16-fish}/templates/default.mustache";

  promptInit = ''
    source ${theme}
    base16-stylix
  '';

in {
  programs.fish.promptInit = promptInit;

  stylix.homeModule = {
    programs.fish.promptInit = promptInit;
  };
}
