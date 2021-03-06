{ pkgs, config, lib, ... }:

let
  # Pin to an older version of Inkscape
  # See https://github.com/nana-4/materia-theme/issues/589
  inkscape-pkgs-source = pkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "ff8752a60361623bdb01f3534f12d503cc854c8e";
    sha256 = "u7lB6n2gOwVWnCE05FDBKQoHG3BqmPtYxVUvvBM8krc=";
  };
  inkscape-pkgs = import inkscape-pkgs-source {
    inherit (pkgs) system;
  };

  materia = with pkgs; stdenvNoCC.mkDerivation {
    name = "stylix-materia";

    src = fetchFromGitHub {
      owner = "nana-4";
      repo = "materia-theme";
      # Newer versions require dart-sass (not yet in nixpkgs)
      rev = "81bbfe6ca61e0b372daa7fada4c2d85752925cfd";
      sha256 = "2Q4RUkrBs27J3Lp1hAzaPpg0gd/0z8zwQBAHhtGBYks=";
    };

    nativeBuildInputs = [
      bc
      inkscape-pkgs.inkscape
      meson
      ninja
      optipng
      sassc
    ];

    FONTCONFIG_FILE = makeFontsConf {
      fontDirectories = [ config.stylix.fonts.sansSerif.package ];
    };

    postPatch = "patchShebangs .";

    # Based on https://github.com/nana-4/materia-theme/blob/master/change_color.sh
    preConfigure =
      with config.lib.stylix.colors;
      let font = config.stylix.fonts.sansSerif.name;
      in ''
      echo 'Setting font family'

      sed 's/$font-family: .*;/$font-family: "${font}";/' \
        -i src/gnome-shell/sass/_variables.scss
      sed 's/$font-family-large: .*;/$font-family-large: "${font}";/' \
        -i src/gnome-shell/sass/_variables.scss

      echo 'Substituting colors'

      PATHLIST=(
        './src/_colors.scss'
        './src/chrome'
        './src/cinnamon'
        './src/cinnamon/assets'
        './src/gnome-shell'
        './src/gtk-2.0/assets.svg'
        './src/gtk-2.0/assets-dark.svg'
        './src/gtk-2.0/gtkrc'
        './src/gtk-2.0/gtkrc-dark'
        './src/gtk-2.0/gtkrc-light'
        './src/gtk-3.0/assets.svg'
        './src/metacity-1'
        './src/unity'
        './src/xfwm4'
      )

      mv src/_colors.scss.template src/_colors.scss

      for FILEPATH in "$PATHLIST"; do
        find "$FILEPATH" -type f -not -name '_color-palette.scss' -exec sed -i'\' \
          -e 's/#121212/%BG%/g' \
          -e 's/#f9f9f9/%BG%/g' \
          -e 's/#000000/%FG%/g' \
          -e 's/#212121/%FG%/g' \
          -e 's/#eeeeee/%FG%/g' \
          -e 's/#ffffff/%FG%/g' \
          -e 's/#272727/%HDR_BG%/g' \
          -e 's/#424242/%HDR_BG%/g' \
          -e 's/#f0f0f0/%HDR_BG%/g' \
          -e 's/#1e1e1e/%HDR_BG2%/g' \
          -e 's/#303030/%HDR_BG2%/g' \
          -e 's/#ebebeb/%HDR_BG2%/g' \
          -e 's/#1d1d1d/%HDR_FG%/g' \
          -e 's/#e4e4e4/%HDR_FG%/g' \
          -e 's/#ffffff/%HDR_FG%/g' \
          -e 's/#565656/%INACTIVE_FG%/g' \
          -e 's/#a7a7a7/%INACTIVE_FG%/g' \
          -e 's/#c1c1c1/%INACTIVE_FG%/g' \
          -e 's/#2e2e2e/%MATERIA_SURFACE%/g' \
          -e 's/#ffffff/%MATERIA_SURFACE%/g' \
          -e 's/#1e1e1e/%MATERIA_VIEW%/g' \
          -e 's/#ffffff/%MATERIA_VIEW%/g' \
          -e 's/#1967d2/%SEL_BG%/g' \
          -e 's/#8ab4f8/%SEL_BG%/g' \
          -e 's/#8ab4f8/%SEL_BG%/g' \
          {} \; ;

        find "$FILEPATH" -type f -exec sed -i'\' \
          -e 's/%BG%/${base00-hash}/g' \
          -e 's/%BG2%/${base01-hash}/g' \
          -e 's/%FG%/${base05-hash}/g' \
          -e 's/%HDR_BG%/${base01-hash}/g' \
          -e 's/%HDR_BG2%/${base01-hash}/g' \
          -e 's/%HDR_BG3%/${base01-hash}/g' \
          -e 's/%HDR_FG%/${base05-hash}/g' \
          -e 's/%INACTIVE_FG%/${base04-hash}/g' \
          -e 's/%MATERIA_SURFACE%/${base01-hash}/g' \
          -e 's/%MATERIA_VIEW%/${base00-hash}/g' \
          -e 's/%SEL_BG%/${base02-hash}/g' \
          -e 's/%SEL_BG2%/${base03-hash}/g' \
          -e 's/%TERMINAL_COLOR4%/${base0A-hash}/g' \
          -e 's/%TERMINAL_COLOR5%/${base0D-hash}/g' \
          -e 's/%TERMINAL_COLOR9%/${base03-hash}/g' \
          -e 's/%TERMINAL_COLOR10%/${base09-hash}/g' \
          -e 's/%TERMINAL_COLOR11%/${base01-hash}/g' \
          -e 's/%TERMINAL_COLOR12%/${base02-hash}/g' \
          -e 's/%MATERIA_SELECTION_OPACITY%/0.32/g' \
          -e 's/%MATERIA_PANEL_OPACITY%/0.6/g' \
          {} \; ;
      done
    '';

    preBuild = ''
      export HOME="$NIX_BUILD_ROOT"
      cd ..

      echo 'Rendering GTK2 assets'
      ./render-assets.sh gtk2-light

      echo 'Rendering GTK3 assets'
      ./render-assets.sh gtk

      cd build
    '';

    mesonFlags = [
      "-Dcolors=default"
      # TODO: Provide an option to choose compact/default
      "-Dsizes=compact"
    ];
  };

  theme = {
    package = materia;
    name = "Materia-compact";
  };

# GTK will probably be unused without Xserver
in lib.mkIf config.services.xserver.enable {
  # Required for Home Manager's GTK settings to work
  services.dbus.packages = [ pkgs.gnome3.dconf ];

  stylix.homeModule = {
    gtk = {
      enable = true;
      inherit theme;
      font = config.stylix.fonts.sansSerif;
    };
  };

  services.xserver.displayManager.lightdm.greeters.gtk.theme = theme;
}
