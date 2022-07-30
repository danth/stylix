{ pkgs, pkgsLib, coricamuLib, config, inputs, ... }:

{
  baseUrl = "https://danth.github.io/stylix/";
  siteTitle = "Stylix";
  language = "en-gb";

  header.html = ''
    <h1>Stylix</h1>
    <nav>
      <a href="">Home</a>
      <a href="options.html">Options list</a>
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
      title = "NixOS Options";

      body.docbook =
        let
          nixosSystem = pkgsLib.nixosSystem {
            inherit (pkgs) system;
            modules = [
              inputs.home-manager.nixosModules.home-manager
              inputs.self.nixosModules.stylix
            ];
          };
        in
          coricamuLib.makeOptionsDocBook {
            inherit (nixosSystem) options;
            customFilter = option: pkgsLib.hasPrefix "stylix" option.name;
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
