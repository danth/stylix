{
  imports = [
    ./deprecated.nix
    ./dev-shell.nix
    ./modules.nix
    ./packages.nix
    ./pre-commit.nix
  ];

  perSystem =
    { pkgs, ... }:
    {
      # TODO: consider using https://flake.parts/options/treefmt-nix.html once
      # dev flake is partitioned
      formatter = pkgs.treefmt.withConfig {
        runtimeInputs = with pkgs; [
          nixfmt-rfc-style
          stylish-haskell
          keep-sorted
        ];

        settings = {
          on-unmatched = "info";
          tree-root-file = "flake.nix";

          formatter = {
            stylish-haskell = {
              command = "stylish-haskell";
              includes = [ "*.hx" ];
            };
            nixfmt = {
              command = "nixfmt";
              options = [ "--width=80" ];
              includes = [ "*.nix" ];
            };
            keep-sorted = {
              command = "keep-sorted";
              includes = [ "*" ];
            };
          };
        };
      };
    };
}
