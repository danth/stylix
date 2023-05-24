{ pkgs, config, ... }:

let
  colors = config.lib.stylix.colors {
    template = builtins.readFile ./colors.mustache;
    extension = "scss";
  };

in pkgs.stdenv.mkDerivation {
  name = "stylix-gnome-shell-theme";

  src = pkgs.fetchFromGitLab {
    domain = "gitlab.gnome.org";
    owner = "GNOME";
    repo = "gnome-shell";
    rev = "44.1";
    sha256 = "h9TBLGakXffEvuP+0dVHmG7+TV5J6CdM/3KHFNUuc70=";
  };

  patches = [ ./shell_colors.patch ];
  postPatch = ''
    rm data/theme/gnome-shell-sass/{_colors.scss,_palette.scss}
    cp ${colors} data/theme/gnome-shell-sass/_colors.scss
  '';

  nativeBuildInputs = with pkgs; [ sass glib.dev ];
  buildPhase = ''
    sass data/theme/gnome-shell.scss >data/theme/gnome-shell.css
    glib-compile-resources --sourcedir=data/theme data/gnome-shell-theme.gresource.xml
  '';

  installPhase = ''
    mkdir -p $out/share/gnome-shell
    mv data/theme/gnome-shell.css $out/share/gnome-shell/gnome-shell.css
    mv data/gnome-shell-theme.gresource $out/share/gnome-shell/gnome-shell-theme.gresource
  '';
}
