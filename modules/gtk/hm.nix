{
  pkgs,
  config,
  lib,
  options,
  ...
}:

let
  cfg = config.stylix.targets.gtk;

  baseCss = config.lib.stylix.colors {
    template = ./gtk.mustache;
    extension = "css";
  };

  finalCss = pkgs.runCommandLocal "gtk.css" { } ''
    cat ${baseCss} >>$out
    echo ${lib.escapeShellArg cfg.extraCss} >>$out
  '';
in
{
  options.stylix.targets.gtk = {
    enable = config.lib.stylix.mkEnableTarget "all GTK3, GTK4 and Libadwaita apps" true;

    extraCss = lib.mkOption {
      description = ''
        Extra code added to `gtk-3.0/gtk.css` and `gtk-4.0/gtk.css`.
      '';
      type = lib.types.lines;
      default = "";
      example = ''
        // Remove rounded corners
        window.background { border-radius: 0; }
      '';
    };

    flatpakSupport.enable = config.lib.stylix.mkEnableTarget "support for theming Flatpak apps" true;
  };

  config = lib.mkIf cfg.enable (
    lib.mkMerge [
      {
        # programs.dconf.enable = true; required in system config
        gtk = {
          enable = true;
          font = {
            inherit (config.stylix.fonts.sansSerif) package name;
            size = config.stylix.fonts.sizes.applications;
          };
          theme = {
            package = pkgs.adw-gtk3;
            name = "adw-gtk3";
          };
        };

        xdg.configFile = {
          "gtk-3.0/gtk.css".source = finalCss;
          "gtk-4.0/gtk.css".source = finalCss;
        };
      }

      (lib.mkIf cfg.flatpakSupport.enable (
        lib.mkMerge [
          {
            # Flatpak apps apparently don't consume the CSS config. This workaround appends it to the theme directly.
            home.file.".themes/${config.gtk.theme.name}".source =
              pkgs.stdenvNoCC.mkDerivation
                {
                  name = "flattenedGtkTheme";
                  src = "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}";

                  installPhase = ''
                    cp --recursive . $out
                    cat ${finalCss} | tee --append $out/gtk-{3,4}.0/gtk.css
                  '';
                };
          }
          (
            let
              filesystem = "${config.home.homeDirectory}/.themes/${config.gtk.theme.name}:ro";
              theme = config.gtk.theme.name;
            in
            if options ? services.flatpak.overrides then
              {
                # Let Flatpak apps read the theme and force them to use it.
                # This requires nix-flatpak to be imported externally.
                services.flatpak.overrides.global = {
                  Context.filesystems = [ filesystem ];
                  Environment.GTK_THEME = theme;
                };
              }
            else
              {
                # This is likely incompatible with other modules that write to this file.
                xdg.dataFile."flatpak/overrides/global".text = ''
                  [Context]
                  filesystems=${filesystem}

                  [Environment]
                  GTK_THEME=${theme}
                '';
              }
          )
        ]
      ))
    ]
  );
}
