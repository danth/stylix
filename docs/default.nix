{
  pkgs,
  lib,
  inputs,
  ...
}:

let
  makeOptionsDoc =
    configuration:
    pkgs.nixosOptionsDoc {
      inherit (configuration) options;

      # Filter out any options not beginning with `stylix`
      transformOptions =
        option:
        option
        // {
          visible = option.visible && builtins.elemAt option.loc 0 == "stylix";
        };
    };

  nixos = makeOptionsDoc (
    lib.nixosSystem {
      inherit (pkgs) system;
      modules = [
        inputs.home-manager.nixosModules.home-manager
        inputs.self.nixosModules.stylix
        ./settings.nix
      ];
    }
  );

  homeManager = makeOptionsDoc (
    inputs.home-manager.lib.homeManagerConfiguration {
      inherit pkgs;
      modules = [
        inputs.self.homeManagerModules.stylix
        ./settings.nix
        {
          home = {
            homeDirectory = "/home/book";
            stateVersion = "22.11";
            username = "book";
          };
        }
      ];
    }
  );

in
pkgs.stdenvNoCC.mkDerivation {
  name = "stylix-book";
  src = ./.;

  patchPhase = ''
    cp ${../README.md} src/README.md
    cp ${../gnome.png} src/gnome.png
    cp ${../kde.png} src/kde.png

    # mdBook doesn't support this Markdown extension yet
    substituteInPlace **/*.md \
      --replace-quiet '> [!NOTE]' '> **Note**' \
      --replace-quiet '> [!TIP]' '> **Tip**' \
      --replace-quiet '> [!IMPORTANT]' '> **Important**' \
      --replace-quiet '> [!WARNING]' '> **Warning**' \
      --replace-quiet '> [!CAUTION]' '> **Caution**'

    # The "declared by" links point to a file which only exists when the docs
    # are built locally. This removes the links.
    sed '/*Declared by:*/,/^$/d' <${nixos.optionsCommonMark} >>src/options/nixos.md
    sed '/*Declared by:*/,/^$/d' <${homeManager.optionsCommonMark} >>src/options/hm.md
  '';

  buildPhase = ''
    ${pkgs.mdbook}/bin/mdbook build --dest-dir $out
  '';
}
