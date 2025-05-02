{
  perSystem =
    {
      pkgs,
      config,
      inputs',
      ...
    }:
    {
      devShells = {
        default =
          let
            check = pkgs.writeShellApplication {
              name = "stylix-check";
              runtimeInputs = with pkgs; [
                nix
                nix-fast-build
              ];
              text = ''
                cores="$(nproc)"
                system="$(nix eval --expr builtins.currentSystem --impure --raw)"
                nix-fast-build \
                  --eval-max-memory-size 512 \
                  --eval-workers "$cores" \
                  --flake ".#checks.$system" \
                  --no-link \
                  --skip-cached \
                  "$@"
              '';
            };
          in
          pkgs.mkShell {
            inherit (config.checks.git-hooks) shellHook;

            packages = [
              check
              inputs'.home-manager.packages.default
              config.checks.git-hooks.enabledPackages
              config.formatter
            ] ++ config.formatter.runtimeInputs;
          };

        ghc = pkgs.mkShell {
          inputsFrom = [ config.devShells.default ];
          packages = [ pkgs.ghc ];
        };
      };
    };
}
