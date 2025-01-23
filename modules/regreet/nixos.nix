{
  pkgs,
  config,
  lib,
  ...
}:

{
  options.stylix.targets.regreet.enable =
    config.lib.stylix.mkEnableTarget "ReGreet" true;

  config =
    lib.mkIf
      (
        config.stylix.enable
        && config.stylix.targets.regreet.enable
        && pkgs.stdenv.hostPlatform.isLinux
      )
      {
        warnings =
          let
            cfg = config.programs.regreet;
          in
          lib.optional
            (
              cfg.enable
              &&
                # defined in https://github.com/NixOS/nixpkgs/blob/8f3e1f807051e32d8c95cd12b9b421623850a34d/nixos/modules/programs/regreet.nix#L153
                config.services.greetd.settings.default_session.command
                != "${pkgs.dbus}/bin/dbus-run-session ${lib.getExe pkgs.cage} ${lib.escapeShellArgs cfg.cageArgs} -- ${lib.getExe cfg.package}"
            )
            "stylix: regreet: custom services.greetd.settings.default_session.command value may not work: ${config.services.greetd.settings.default_session.command}";
        programs.regreet = {
          settings.GTK.application_prefer_dark_theme = config.stylix.polarity == "dark";
          settings.background = {
            path = config.stylix.image;
            fit =
              let
                inherit (config.stylix) imageScalingMode;
              in
              if imageScalingMode == "fill" then
                "Cover"
              else if imageScalingMode == "fit" then
                "Contain"
              else if imageScalingMode == "stretch" then
                "Fill"
              # No other available options
              else
                null;
          };
          font = {
            inherit (config.stylix.fonts.sansSerif) name package;
          };
          cursorTheme = {
            inherit (config.stylix.cursor) name package;
          };
          theme = {
            package = pkgs.adw-gtk3;
            name = "adw-gtk3";
          };
        };
      };
}
