{
  stdenv,
  sass,
  glib,
  colors,
  templates,
}:

let
  colorsScss = colors {
    template = ./colors.mustache;
    extension = "scss";
  };

in
stdenv.mkDerivation {
  name = "${colors.slug}-gnome-shell-theme";
  src = templates.gnome-shell;
  patches = [ ./shell_colors.patch ];
  postPatch = ''
    rm data/theme/gnome-shell-sass/{_colors.scss,_default-colors.scss,_palette.scss}
    cp ${colorsScss} data/theme/gnome-shell-sass/_colors.scss
  '';

  nativeBuildInputs = [
    sass
    glib.dev
  ];
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
