{ pkgs, config, lib, ... }:

let
  rendersvg = pkgs.runCommandLocal "rendersvg" { } ''
    mkdir -p $out/bin
    ln -s ${pkgs.resvg}/bin/resvg $out/bin/rendersvg
  '';

  materia = with pkgs;
    stdenvNoCC.mkDerivation {
      name = "stylix-materia";

      src = fetchFromGitHub {
        owner = "nana-4";
        repo = "materia-theme";
        # Newer versions require dart-sass (not yet in nixpkgs)
        rev = "81bbfe6ca61e0b372daa7fada4c2d85752925cfd";
        sha256 = "2Q4RUkrBs27J3Lp1hAzaPpg0gd/0z8zwQBAHhtGBYks=";
      };

      nativeBuildInputs = [ bc meson ninja optipng rendersvg sassc ];

      FONTCONFIG_FILE = makeFontsConf {
        fontDirectories = [ config.stylix.fonts.sansSerif.package ];
      };

      postPatch = ''
        patchShebangs .
        sed -e '/handle-horz-.*/d' -e '/handle-vert-.*/d' -i src/gtk-2.0/assets.txt
      '';

      # Based on https://github.com/nana-4/materia-theme/blob/master/change_color.sh
      preConfigure = with config.lib.stylix.colors.withHashtag;
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
              -e 's/%BG%/${base00}/g' \
              -e 's/%BG2%/${base01}/g' \
              -e 's/%FG%/${base05}/g' \
              -e 's/%HDR_BG%/${base01}/g' \
              -e 's/%HDR_BG2%/${base01}/g' \
              -e 's/%HDR_BG3%/${base01}/g' \
              -e 's/%HDR_FG%/${base05}/g' \
              -e 's/%INACTIVE_FG%/${base04}/g' \
              -e 's/%MATERIA_SURFACE%/${base01}/g' \
              -e 's/%MATERIA_VIEW%/${base00}/g' \
              -e 's/%SEL_BG%/${base02}/g' \
              -e 's/%SEL_BG2%/${base03}/g' \
              -e 's/%TERMINAL_COLOR4%/${base0A}/g' \
              -e 's/%TERMINAL_COLOR5%/${base0D}/g' \
              -e 's/%TERMINAL_COLOR9%/${base03}/g' \
              -e 's/%TERMINAL_COLOR10%/${base09}/g' \
              -e 's/%TERMINAL_COLOR11%/${base01}/g' \
              -e 's/%TERMINAL_COLOR12%/${base02}/g' \
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

in {
  options.stylix.targets.gtk.enable =
    config.lib.stylix.mkEnableTarget "the GTK theme"
    # GTK will probably be unused without Xorg / Wayland.
    # There isn't a single option which covers all Wayload compositors,
    # and many of them don't have NixOS modules at all. Therefore, we use
    # OpenGL as the next best condition to detect that Wayland is enabled.
    (config.services.xserver.enable || config.hardware.opengl.enable);

  config = lib.mkIf config.stylix.targets.gtk.enable {
    # Required for Home Manager's GTK settings to work
    programs.dconf.enable = true;

    home-manager.sharedModules = [{
      gtk = {
        enable = true;
        inherit theme;
        font = config.stylix.fonts.sansSerif;
      };
    }];

    services.xserver.displayManager.lightdm.greeters.gtk.theme = theme;
  };
}
