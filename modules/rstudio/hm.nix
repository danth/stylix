{
  pkgs,
  config,
  lib,
  ...
}:
let
  hexToRGB =
    hexString:
    let
      inherit (lib.trivial) fromHexString;
    in
    {
      r = fromHexString (builtins.substring 0 2 hexString);
      g = fromHexString (builtins.substring 2 2 hexString);
      b = fromHexString (builtins.substring 4 2 hexString);
    };
in
{

  options.stylix.targets.rstudio.enable =
    config.lib.stylix.mkEnableTarget "RStudio" true;

  config =
    lib.mkIf (config.stylix.enable && config.stylix.targets.rstudio.enable)
      {
        xdg.configFile."rstudio/themes/stylix.rstheme" =
          let
            rgb = hexToRGB config.lib.stylix.colors.base00;
            luminance = (0.2126 * rgb.r) + (0.7152 * rgb.g) + (0.0722 * rgb.b);
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
