{
  pkgs,
  config,
  lib,
  ...
}:

let
  cfg = config.stylix.targets.grub;
  inherit (config.stylix) imageScalingMode fonts;
  inherit (config.lib.stylix) mkEnableTarget mkEnableWallpaper pixel;
  # Grub requires fonts to be converted to "PFF2 format"
  # This function takes a font { name, package } and produces a .pf2 file
  mkGrubFont =
    font:
    pkgs.runCommand "${font.package.name}.pf2"
      {
        FONTCONFIG_FILE = pkgs.makeFontsConf { fontDirectories = [ font.package ]; };
      }
      ''
        # Use fontconfig to select the correct .ttf or .otf file based on name
        font=$(
          ${lib.getExe' pkgs.fontconfig "fc-match"} \
          ${lib.escapeShellArg font.name} \
          --format=%{file}
        )

        # Convert to .pf2
        ${pkgs.grub2}/bin/grub-mkfont $font --output $out --size ${toString fonts.sizes.applications}
      '';

  image-scale =
    if imageScalingMode == "fill" then
      "crop"
    else if imageScalingMode == "fit" then
      "fitheight"
    else if imageScalingMode == "center" then
      "padding"
    # Grub doesn't seem to support tile
    else
      "crop";
in
{
  imports = [
    (lib.mkRenamedOptionModuleWith {
      from = [
        "stylix"
        "targets"
        "grub"
        "useImage"
      ];
      sinceRelease = 2505;
      to = [
        "stylix"
        "targets"
        "grub"
        "useWallpaper"
      ];
    })
  ];
  options.stylix.targets.grub = {
    enable = mkEnableTarget "GRUB" true;
    useWallpaper = mkEnableWallpaper "GRUB" false;
  };

  config.boot.loader.grub =
    with config.lib.stylix.colors.withHashtag;
    lib.mkIf (config.stylix.enable && cfg.enable) {
      backgroundColor = base00;
      # Need to override the NixOS splash, this will match the background
      splashImage = pixel "base00";

      # This font will be used for the GRUB terminal
      font = toString (mkGrubFont fonts.monospace);

      # TODO: Include OS icons
      theme =
        pkgs.runCommand "stylix-grub"
          {
            themeTxt = ''
              desktop-image: "background.png"
              desktop-image-scale-method: "${image-scale}"
              desktop-color: "${base00}"

              title-text: ""

              terminal-left: "10%"
              terminal-top: "20%"
              terminal-width: "80%"
              terminal-height: "60%"

              + progress_bar {
                left = 25%
                top = 80%+20  # 20 pixels below boot menu
                width = 50%
                height = 30

                id = "__timeout__"
                show_text = true
                font = "${fonts.sansSerif.name}"
                text = "@TIMEOUT_NOTIFICATION_MIDDLE@"

                border_color = "${base00}"
                bg_color = "${base00}"
                fg_color = "${base0B}"
                text_color = "${base05}"
              }

              + boot_menu {
                left = 25%
                top = 20%
                width = 50%
                height = 60%
                menu_pixmap_style = "background_*.png"

                item_height = 40
                item_icon_space = 8
                item_spacing = 0
                item_padding = 0
                item_font = "${fonts.sansSerif.name}"
                item_color = "${base05}"

                selected_item_color = "${base01}"
                selected_item_pixmap_style = "selection_*.png"
              }
            '';
            passAsFile = [ "themeTxt" ];
          }
          ''
            mkdir $out
            cp $themeTxtPath $out/theme.txt

            ${
              if
                cfg.useWallpaper
              # Make sure the background image is .png by asking to convert it
              then
                "${pkgs.imagemagick}/bin/convert ${config.stylix.image} png32:$out/background.png"
              else
                "cp ${pixel "base00"} $out/background.png"
            }

            cp ${pixel "base01"} $out/background_c.png
            cp ${pixel "base0B"} $out/selection_c.png

            cp ${mkGrubFont fonts.sansSerif} $out/sans_serif.pf2
          '';
    };
}
