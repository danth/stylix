{ inputs, ... }:
{
  perSystem =
    {
      lib,
      system,
      config,
      pkgs,
      ...
    }:
    {
      checks = lib.mkMerge [
        config.packages
        {
          # TODO: consider using https://flake.parts/options/git-hooks-nix.html
          git-hooks = inputs.git-hooks.lib.${system}.run {
            hooks = {
              deadnix.enable = true;
              editorconfig-checker.enable = true;
              hlint.enable = true;

              treefmt = {
                enable = true;
                package = config.formatter;
              };

              statix.enable = true;
              typos = {
                enable = true;
                settings.configuration = ''
                  [default.extend-identifiers]
                  MrSom3body="MrSom3body"
                '';
              };
              yamllint.enable = true;
            };

            src = ./.;
          };

          maintainers-sorted =
            pkgs.callPackage ../../stylix/check-maintainers-sorted.nix
              { };
        }
      ];
    };
}
