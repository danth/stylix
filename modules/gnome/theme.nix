{ pkgs, config, ... }:

let
  colors = config.lib.stylix.colors {
    template = ./colors.mustache;
    extension = "scss";
  };

in pkgs.stdenv.mkDerivation {
  name = "${config.lib.stylix.colors.slug}-gnome-shell-theme";

  src = pkgs.fetchFromGitLab {
    domain = "gitlab.gnome.org";
    owner = "GNOME";
    repo = "gnome-shell";
    rev = "45.1";
    sha256 = "/TIkZ8y5Wv3QHLFp79Poao9fINurKs5pa4z0CRe+F8s=";
  };

  patches = [ ./shell_colors.patch ];
  postPatch = ''
    rm data/theme/gnome-shell-sass/{_colors.scss,_palette.scss}
    cp ${colors} data/theme/gnome-shell-sass/_colors.scss
  '';

  nativeBuildInputs = with pkgs; [ sass glib.dev ];
  buildPhase = ''
    sass data/theme/gnome-shell-light.scss \
      >data/theme/gnome-shell-light.css
    cp data/theme/gnome-shell-{light,dark}.css
    glib-compile-resources \
      --sourcedir=data/theme \
      data/gnome-shell-theme.gresource.xml
  '';

  installPhase = ''
    mkdir -p $out/share/gnome-shell
    mv data/theme/gnome-shell-light.css $out/share/gnome-shell/gnome-shell.css
    mv data/gnome-shell-theme.gresource $out/share/gnome-shell/gnome-shell-theme.gresource
  '';
}
