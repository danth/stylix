{
  pkgs,
  config,
  lib,
  ...
}: {
  options.stylix.targets.kde.enable =
    config.lib.stylix.mkEnableTarget "KDE" pkgs.stdenv.hostPlatform.isLinux;

  config = lib.mkIf config.stylix.targets.kde.enable (let
    kscreenlockerrc = ''
      [Greeter][Wallpaper][org.kde.image][General][$i]
      Image=${config.stylix.image}
    '';

    configDir =
      pkgs.runCommandLocal "stylix-kde"
      {
        inherit kscreenlockerrc;
      }
      ''
        mkdir $out
        echo "$kscreenlockerrc" >$out/kscreenlockerrc
      '';

    script = ''
      for (desktop of desktops()) {
        desktop.wallpaperPlugin = "org.kde.image";
        desktop.currentConfigGroup = ["Wallpaper", "org.kde.image", "General"];
        desktop.writeConfig("Image", "${config.stylix.image}");
      }
    '';
  in {
    # We can't just link config files within the home directory because
    # they're expected to be writeable.
    xdg.systemDirs.config = ["${configDir}"];

    # This fails silently unless Plasma is running.
    home.activation.kdeWallpaper = lib.hm.dag.entryAfter ["writeBoundary"] ''
      ${pkgs.dbus}/bin/dbus-send \
        --type=method_call \
        --dest=org.kde.plasmashell \
        /PlasmaShell \
        org.kde.PlasmaShell.evaluateScript \
        string:${lib.escapeShellArg script} \
        || true
    '';
  });
}
