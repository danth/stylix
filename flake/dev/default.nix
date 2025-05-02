{
  imports = [
    ./checks.nix
    ./devShell.nix
    ./packages.nix
  ];

  perSystem =
    { pkgs, ... }:
    {
      formatter = pkgs.treefmt.withConfig {
        runtimeInputs = with pkgs; [
          nixfmt-rfc-style
          stylish-haskell
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
          };
        };
      };
    };
}
