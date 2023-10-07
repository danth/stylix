{ pkgs, config, lib, ... }:

{
  options.stylix.targets.fish.enable =
    config.lib.stylix.mkEnableTarget "Fish" true;

  config = lib.mkIf config.stylix.targets.fish.enable {
    programs.fish = {
      interactiveShellInit = "base16-${config.lib.stylix.colors.slug}";
      plugins = [{
        name = "stylix";
        src = config.lib.stylix.colors {
          templateRepo = pkgs.fetchFromGitHub {
            owner = "tomyun";
            repo = "base16-fish";
            rev = "7f647967fddedaf803191bc9113b13d2071dc3cf";
            sha256 = "IGUbLjsmmAvB9UKGkR7oqdpjeVEfzt83GpyBkrZf2O4=";
          };
        };
      }];
    };
  };
}
