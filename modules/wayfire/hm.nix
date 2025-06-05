{
  mkTarget,
  config,
  lib,
  pkgs,
  ...
}:
mkTarget {
  name = "wayfire";
  humanName = "Wayfire";

  extraOptions.useWallpaper = config.lib.stylix.mkEnableWallpaper "wayfire" true;

  configElements = [
    (
      { fonts }:
      {
        wayland.windowManager.wayfire.settings.decoration.font =
          "${fonts.monospace.name} ${toString fonts.sizes.desktop}";
      }
    )
    (
      {
        cfg,
        image,
        imageScalingMode,
      }:
      let
        wayfireBackground = pkgs.runCommand "wayfire-background.png" { } ''
          ${lib.getExe' pkgs.imagemagick "convert"} ${image} $out
        '';
      in
      {
        wayland.windowManager.wayfire.wf-shell.settings = {
          background.image = lib.mkIf cfg.useWallpaper (toString wayfireBackground);
          cube = {
            cubemap_image = lib.mkIf cfg.useWallpaper (toString wayfireBackground);
            skydome_texture = lib.mkIf cfg.useWallpaper (toString wayfireBackground);
          };
          background.fill_mode =
            if imageScalingMode == "stretch" then
              "stretch"
            else if imageScalingMode == "fit" then
              "preserve_aspect"
            else
              "fill_and_crop";

        };
      }
    )
    (lib.mkIf config.stylix.targets.nixos-icons.enable {
      wayland.windowManager.wayfire.wf-shell.settings.panel.menu_icon =
        "${pkgs.nixos-icons}/share/icons/hicolor/256x256/apps/nix-snowflake.png";
    })
    (
      { colors }:
      let
        rgba = rgb: a: "\\#${rgb}${a}";
        rgb = (lib.flip rgba) "ff";
      in
      {
        wayland.windowManager.wayfire = {
          settings = {
            cube = {
              background = rgb colors.base00;
            };

            expo.background = rgb colors.base00;
            vswitch.background = rgb colors.base00;
            vswipe.background = rgb colors.base00;
            core.background_color = rgb colors.base00;

            decoration = {
              active_color = rgb colors.base0D;
              inactive_color = rgb colors.base03;
            };
          };

          wf-shell.settings = {
            panel.background_color = rgb colors.base01;
          };
        };
      }
    )
  ];
}
