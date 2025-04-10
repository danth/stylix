{
  pkgs,
  config,
  lib,
  ...
}:
{
  options.stylix.targets.rstudio.enable =
    config.lib.stylix.mkEnableTarget "RStudio" true;

  config =
    lib.mkIf (config.stylix.enable && config.stylix.targets.rstudio.enable)
      {
        xdg.configFile."rstudio/themes/stylix.rstheme" = {
          source = config.lib.stylix.colors {
            template = ./base.rstheme.mustache;
            extension = ".rstheme";
          };
        };

        home.activation.rstudioThemeSelect =
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
                '.editor_theme |= "stylixBase16"' \
                "$config" |
                ${lib.getExe' pkgs.moreutils "sponge"} "$config"

              verboseEcho \
                "stylix: rstudio: setting editor_theme to '${name}' in $config"

            else
              run cp ${file} "$config"
            fi
          '';
      };
}
