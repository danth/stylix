{ haskellPackages, stdenvNoCC }:

let
  ghc = haskellPackages.ghcWithPackages (
    ps: with ps; [
      JuicyPixels
      json
      random
      vector-algorithms
    ]
  );

  # `nix build .#palette-generator.passthru.docs` and open in a web browser
  docs = stdenvNoCC.mkDerivation {
    name = "palette-generator-haddock";

    src = ./.;
    buildInputs = [ ghc ];

    buildPhase = ''
      haddock $src/**/*.hs --html --ignore-all-exports --odir $out
    '';
    dontInstall = true;
    dontFixup = true;
  };

in
stdenvNoCC.mkDerivation {
  name = "palette-generator";

  src = ./.;
  buildInputs = [ ghc ];

  buildPhase = ''
    ghc -O -threaded -Wall -Wno-type-defaults Stylix/Main.hs
  '';
  installPhase = ''
    install -D Stylix/Main $out/bin/palette-generator
  '';

  passthru = { inherit docs; };
}
