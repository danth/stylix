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
          biome
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
            biome = {
              command = "biome";
              options =
                let
                  settings = {
                    formatter = {
                      indentStyle = "space";
                      indentWidth = 2;
                      lineWidth = 80;
                    };
                  };
                in
                [
                  "format"
                  "--write"
                  "--no-errors-on-unmatched"
                  "--config-path"
                  (pkgs.writers.writeJSON "biome.json" settings)
                ];
              includes = [
                "*.js"
                "*.ts"
                # TODO: other supported filetypes
                # "*.mjs"
                # "*.mts"
                # "*.cjs"
                # "*.cts"
                # "*.jsx"
                # "*.tsx"
                # "*.d.ts"
                # "*.d.cts"
                # "*.d.mts"
                # "*.json"
                # "*.jsonc"
                # "*.css"
              ];
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
