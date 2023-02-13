{ pkgs, pkgsLib, coricamuLib, config, inputs, ... }:

{
  baseUrl = "https://danth.github.io/stylix/";
  siteTitle = "Stylix";
  language = "en-gb";

  header.html = ''
    <h1>Stylix</h1>
    <nav>
      <a href="">Home</a>
      <a href="options.html">NixOS options</a>
      <a href="options-hm.html">Home Manager options</a>
      <a href="haddock/doc-index.html">Haskell internals</a>
      <a href="https://github.com/danth/stylix">GitHub repository</a>
    </nav>
  '';

  pages = [
    {
      path = "index.html";
      title = "Stylix";

      body.markdownFile = pkgs.runCommand "index.md" {} ''
        # Remove the title line
        tail -n+2 ${../README.md} >$out
      '';
    }

    {
      path = "options.html";
      title = "NixOS options";

      body.docbook =
        let
          configuration = pkgsLib.nixosSystem {
            inherit (pkgs) system;
            modules = [
              inputs.self.nixosModules.stylix
              ./settings.nix
              { _module.check = false; }
            ];
          };
        in
          coricamuLib.makeOptionsDocBook {
            inherit (configuration) options;
            customFilter = option: builtins.elemAt option.loc 0 == "stylix";
          };
    }

    {
      path = "options-hm.html";
      title = "Home Manager options";

      body.docbook =
        let
          configuration = inputs.home-manager.lib.homeManagerConfiguration {
            inherit pkgs;
            modules = [
              inputs.self.homeManagerModules.stylix
              ./settings.nix
              {
                home = {
                  homeDirectory = "/home/docs";
                  stateVersion = "22.11";
                  username = "docs";
                };
              }
            ];
            check = false;
          };
        in
          coricamuLib.makeOptionsDocBook {
            inherit (configuration) options;
            customFilter = option: builtins.elemAt option.loc 0 == "stylix";
          };
    }
  ];

  files.haddock =
    let
      ghc = pkgs.haskellPackages.ghcWithPackages
        (ps: with ps; [ json JuicyPixels random ]);
    in
      pkgs.stdenvNoCC.mkDerivation {
        name = "palette-generator-haddock";
        src = ../palette-generator;
        buildInputs = [ ghc ];
        buildPhase = ''
          haddock $src/**/*.hs \
            --html \
            --ignore-all-exports \
            --use-contents '${config.baseUrl}' \
            --odir $out
        '';
        dontInstall = true;
        dontFixup = true;
      };
}
