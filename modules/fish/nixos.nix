{ pkgs, config, lib, ... }:

{
  options.stylix.targets.fish.enable =
    config.lib.stylix.mkEnableTarget "Fish" true;

  config = lib.mkIf config.stylix.targets.fish.enable {
    environment.etc."fish/stylix.fish".source = config.lib.stylix.colors {
      templateRepo = pkgs.fetchFromGitHub {
        owner = "tomyun";
        repo = "base16-fish";
        rev = "7f647967fddedaf803191bc9113b13d2071dc3cf";
        sha256 = "IGUbLjsmmAvB9UKGkR7oqdpjeVEfzt83GpyBkrZf2O4=";
      };
    };
    programs.fish.promptInit = ''
      source /etc/fish/stylix.fish
      base16-${config.lib.stylix.colors.slug}
    '';
  };
}
