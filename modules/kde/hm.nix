{ pkgs, config, lib, ... }:

with config.stylix.fonts;
with config.lib.stylix.colors;

let
  colors = {
    BackgroundNormal = "${base00-rgb-r},${base00-rgb-g},${base00-rgb-b}";
    BackgroundAlternate = "${base01-rgb-r},${base01-rgb-g},${base01-rgb-b}";

    DecorationFocus = "${base01-rgb-r},${base01-rgb-g},${base01-rgb-b}";
    DecorationHover = "${base01-rgb-r},${base01-rgb-g},${base01-rgb-b}";

    ForegroundNormal = "${base05-rgb-r},${base05-rgb-g},${base05-rgb-b}";
    ForegroundActive = "${base05-rgb-r},${base05-rgb-g},${base05-rgb-b}";
    ForegroundInactive = "${base05-rgb-r},${base05-rgb-g},${base05-rgb-b}";
    ForegroundLink = "${base05-rgb-r},${base05-rgb-g},${base05-rgb-b}";
    ForegroundNegative = "${base08-rgb-r},${base08-rgb-g},${base08-rgb-b}";
    ForegroundNeutral = "${base0D-rgb-r},${base0D-rgb-g},${base0D-rgb-b}";
    ForegroundPositive = "${base0B-rgb-r},${base0B-rgb-g},${base0B-rgb-b}";
    ForegroundVisited = "${base05-rgb-r},${base05-rgb-g},${base05-rgb-b}";
  };

  colorEffect = {
    ColorEffect = 0;
    ColorAmount = 0;

    ContrastEffect = 1;
    ContrastAmount = 0.5;

    IntensityEffect = 0;
    IntensityAmount = 0;
  };

  icons = {
    Animated = false;

    ActiveColor = "${base0A-rgb-r},${base0A-rgb-g},${base0A-rgb-b}";
    ActiveColor2 = "${base0A-rgb-r},${base0A-rgb-g},${base0A-rgb-b}";
    ActiveEffect = "none";
    ActiveSemiTransparent = false;
    ActiveValue = 1;

    DefaultColor = "${base05-rgb-r},${base05-rgb-g},${base05-rgb-b}";
    DefaultColor2 = "${base05-rgb-r},${base05-rgb-g},${base05-rgb-b}";
    DefaultEffect = "none";
    DefaultSemiTransparent = false;
    DefaultValue = 1;

    DisabledColor = "${base03-rgb-r},${base03-rgb-g},${base03-rgb-b}";
    DisabledColor2 = "${base03-rgb-r},${base03-rgb-g},${base03-rgb-b}";
    DisabledEffect = "none";
    DisabledSemiTransparent = true;
    DisabledValue = 1;
  };

  desktopFont = "${sansSerif.name},${toString sizes.desktop},-1,5,50,0,0,0,0,0";
  applicationFont = "${sansSerif.name},${toString sizes.applications},-1,5,50,0,0,0,0,0";
  monospaceFont = "${monospace.name},${toString sizes.terminal},-1,5,50,0,0,0,0,0";

  kdeglobals = {
    # The existence of this group makes the following settings unable to
    # be changed by the user, as specified at
    # https://develop.kde.org/docs/administration/kiosk/introduction/
    "$i" = {};

    General = {
      Name = scheme-name;
      ColorScheme = "Breeze";
      widgetStyle = "Oxygen";

      inherit desktopFont;
      fixed = monospaceFont;
      font = applicationFont;
      menuFont = desktopFont;
      taskbarFont = desktopFont;
      toolBarFont = desktopFont;
      smallestReadableFont = desktopFont;
    };

    "ColorEffects:Disabled" = colorEffect;
    "ColorEffects:Inactive" = colorEffect;

    "Colors:Button" = colors;
    "Colors:Complementary" = colors;
    "Colors:Selection" = colors // {
      BackgroundNormal = "${base01-rgb-r},${base01-rgb-g},${base01-rgb-b}";
    };
    "Colors:Tooltip" = colors;
    "Colors:View" = colors;
    "Colors:Window" = colors;

    WM = {
      activeBlend = "${base0A-rgb-r},${base0A-rgb-g},${base0A-rgb-b}";
      activeBackground = "${base00-rgb-r},${base00-rgb-g},${base00-rgb-b}";
      activeForeground = "${base05-rgb-r},${base05-rgb-g},${base05-rgb-b}";
      inactiveBlend = "${base03-rgb-r},${base03-rgb-g},${base03-rgb-b}";
      inactiveBackground = "${base00-rgb-r},${base00-rgb-g},${base00-rgb-b}";
      inactiveForeground = "${base05-rgb-r},${base05-rgb-g},${base05-rgb-b}";

      activeFont = desktopFont;
    };

    DesktopIcons = icons;
    MainToolbarIcons = icons;
    PanelIcons = icons;
    SmallIcons = icons;
    ToolbarIcons = icons;
  };

  kscreenlockerrc = ''
    [Greeter][Wallpaper][org.kde.image][General][$i]
    Image=${config.stylix.image}
  '';

  configDir = pkgs.runCommandLocal "stylix-kde"
    {
      kdeglobals = (pkgs.formats.ini {}).generate "kdeglobals" kdeglobals;
      inherit kscreenlockerrc;
    }
    ''
      mkdir $out
      cp $kdeglobals $out/kdeglobals
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
  options.stylix.targets.kde.enable =
    config.lib.stylix.mkEnableTarget "KDE" true;

  config = lib.mkIf config.stylix.targets.kde.enable {
    qt = {
      enable = true;
      style.name = "breeze";
    };

    # We can't just link config files within the home directory because
    # they're expected to be writeable.
    xdg.systemDirs.config = [ "${configDir}" ];

    # This fails silently unless Plasma is running.
    home.activation.kdeWallpaper =
      lib.hm.dag.entryAfter ["writeBoundary"] ''
        ${pkgs.dbus}/bin/dbus-send \
          --type=method_call \
          --dest=org.kde.plasmashell \
          /PlasmaShell \
          org.kde.PlasmaShell.evaluateScript \
          string:${lib.escapeShellArg script} \
          || true
      '';
  };
}
