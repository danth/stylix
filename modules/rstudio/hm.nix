{
  pkgs,
  config,
  lib,
  ...
}:
{

  options.stylix.targets.rstudio = {
    enable = config.lib.stylix.mkEnableTarget "RStudio" true;

    themeAutoset = lib.mkOption {
      description = ''
        Whether to modify the RStudio preferences file to set the theme.
      '';
      type = lib.types.bool;
      default = true;
    };
  };

  config =
    lib.mkIf (config.stylix.enable && config.stylix.targets.rstudio.enable)
      {
        xdg.configFile."rstudio/themes/stylix.rstheme" =
          let
            inherit (config.lib.stylix) colors;
            luminance =
              (0.2126 * (lib.toInt colors.base00-rgb-r))
              + (0.7152 * (lib.toInt colors.base00-rgb-g))
              + (0.0722 * (lib.toInt colors.base00-rgb-b));
            polarity =
              if luminance < 128 then
                "/* rs-theme-is-dark: TRUE */"
              else
                "/* rs-theme-is-dark: FALSE */";
            mustache =
              ''
                /* rsthemes 0.5.0 */
                /* https://github.com/danth/stylix */
                /* rs-theme-name: stylixBase16*/
                /* Theme Generator by Grady B  */
              ''
              + polarity
              + "\n"
              + builtins.readFile ./base.rstheme.mustache;

          in
          {
            source = config.lib.stylix.colors {
              template = mustache;
              extension = ".rstheme";
            };
          };
        home.activation.rstudioThemeSelect =
          if config.stylix.targets.rstudio.themeAutoset then
            let
              file = builtins.toFile "rstudio-prefs.json" (
                builtins.toJSON { editor_theme = name; }
              );
              name = "stylixBase16";
            in

            lib.hm.dag.entryAfter [ "writeBoundary" ] ''
              config="$HOME/.config/rstudio/rstudio-prefs.json"

              if [[ -f "$config" ]]; then
                run ${lib.getExe pkgs.jq} \
                  --raw-output \
                  '.editor_theme |= "${name}"' \
                  "$config" |
                  ${lib.getExe' pkgs.moreutils "sponge"} "$config"

                verboseEcho \
                  "stylix: rstudio: changing editor_theme to ${name} in $config"
              else
                run cp ${file} "$config"
                run chmod 644 "$config"
              fi
            ''
          else
            lib.hm.dag.entryAfter ''

              verboseEcho \
                "stylix: rstudio: not changing editor_theme: option disabled"
            '';
      };
}
