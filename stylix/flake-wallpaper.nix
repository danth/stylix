{ pkgs, config, ... }:
{ width
, height
, logoScale
, backgroundColor ? config.lib.stylix.colors.withHashtag.base01
, logoColor1 ? config.lib.stylix.colors.withHashtag.base0C
, logoColor2 ? config.lib.stylix.colors.withHashtag.base0D
}:
pkgs.stdenv.mkDerivation {
  name = "generated-nix-wallpaper-${config.lib.stylix.colors.slug}.png";
  src = pkgs.writeTextFile {
    name = "template.svg";
    text = ''
      <svg width="${toString width}" height="${
        toString height
      }" version="1.1" xmlns="http://www.w3.org/2000/svg">
        <rect width="${toString width}" height="${
          toString height
        }" fill="${backgroundColor}"/>
        <svg x="${toString (width / 2 - (logoScale * 50))}" y="${
          toString (height / 2 - (logoScale * 50))
        }" version="1.1" xmlns="http://www.w3.org/2000/svg">
          <g transform="scale(${toString logoScale})">
            <g transform="matrix(.19936 0 0 .19936 80.161 27.828)">
              <path d="m-53.275 105.84-122.2-211.68 56.157-0.5268 32.624 56.869 32.856-56.565 27.902 0.011 14.291 24.69-46.81 80.49 33.229 57.826zm-142.26 92.748 244.42 0.012-27.622 48.897-65.562-0.1813 32.559 56.737-13.961 24.158-28.528 0.031-46.301-80.784-66.693-0.1359zm-9.3752-169.2-122.22 211.67-28.535-48.37 32.938-56.688-65.415-0.1717-13.942-24.169 14.237-24.721 93.111 0.2937 33.464-57.69z" fill="${logoColor1}"/>
              <path d="m-97.659 193.01 122.22-211.67 28.535 48.37-32.938 56.688 65.415 0.1716 13.941 24.169-14.237 24.721-93.111-0.2937-33.464 57.69zm-9.5985-169.65-244.42-0.012 27.622-48.897 65.562 0.1813-32.559-56.737 13.961-24.158 28.528-0.031 46.301 80.784 66.693 0.1359zm-141.76 93.224 122.2 211.68-56.157 0.5268-32.624-56.869-32.856 56.565-27.902-0.011-14.291-24.69 46.81-80.49-33.229-57.826z" fill="${logoColor2}" style="isolation:auto;mix-blend-mode:normal"/>
            </g>
          </g>
        </svg>
      </svg>
    '';
  };
  buildInputs = with pkgs; [ inkscape ];
  unpackPhase = "true";
  buildPhase = ''
    inkscape --export-type="png" $src -w ${toString width} -h ${
      toString height
    } -o wallpaper.png
  '';
  installPhase = "install -Dm0644 wallpaper.png $out";
}
