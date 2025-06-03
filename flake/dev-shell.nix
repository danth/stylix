{
  perSystem =
    {
      pkgs,
      config,
      inputs',
      ...
    }:
    let
      stylix-check = pkgs.writeShellApplication {
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

      # The shell should not directly depend on `packages.serve-docs`, because
      # that'd build the docs before entering the shell. Instead, we want to
      # build the docs only when running 'serve-docs'.
      #
      # For a similar reason, we can't use `self` as a reference to the flake:
      # `self` represents the flake as it was when the devshell was evaluated,
      # not the local flake worktree that has possibly been modified since
      # entering the devshell.
      build-and-run-docs = pkgs.writeShellScriptBin "serve-docs" ''
        nix run .#doc
      '';
    in
    {
      devShells = {
        default = pkgs.mkShell {
          # Install git-hooks when activating the shell
          shellHook = config.pre-commit.installationScript;

          packages =
            [
              stylix-check
              build-and-run-docs
              inputs'.home-manager.packages.default
              config.formatter
            ]
            ++ config.pre-commit.settings.enabledPackages
            ++ config.formatter.runtimeInputs;
        };

        ghc = pkgs.mkShell {
          inputsFrom = [ config.devShells.default ];
          packages = [ pkgs.ghc ];
        };
      };
    };
}
