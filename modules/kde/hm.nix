{ pkgs, config, lib, ... }:

with config.stylix.fonts;
with config.lib.stylix.colors;

let
  formatValue = value:
    if builtins.isBool value
    then if value then "true" else "false"
    else builtins.toString value;

  formatSection = path: data:
    let
      header = lib.concatStrings (map (p: "[${p}]") path);
      formatChild = name: formatLines (path ++ [ name ]);
      children = lib.mapAttrsToList formatChild data;
      partitioned = lib.partition builtins.isString children;
      directChildren = partitioned.right;
      indirectChildren = partitioned.wrong;
    in
      lib.optional (directChildren != []) header ++
      directChildren ++
      lib.flatten indirectChildren;

  formatLines = path: data:
    if builtins.isAttrs data
    then
      if data?_immutable
      then
        if builtins.isAttrs data.value
        then formatSection (path ++ [ "$i" ]) data.value
        else "${lib.last path}[$i]=${formatValue data.value}"
      else formatSection path data
    else "${lib.last path}=${formatValue data}";

  formatConfig = data:
    lib.concatStringsSep "\n" (formatLines [] data);

  # Marking a setting as immutable should prevent it being overwritten
  # through the system settings menu.
  makeImmutable = value: {
    _immutable = true;
    inherit value;
  };

  # PascalCase is the standard naming for color scheme files. Schemes named
  # in kebab-case will load when selected manually, but don't work with a
  # look and feel package.
  colorschemeSlug = lib.concatStrings
    (builtins.filter builtins.isString
      (builtins.split "[^a-zA-Z]" scheme));

  colorEffect = {
    ColorEffect = 0;
    ColorAmount = 0;
    ContrastEffect = 1;
    ContrastAmount = 0.5;
    IntensityEffect = 0;
    IntensityAmount = 0;
  };

  colors = {
    BackgroundNormal = "${base00-rgb-r},${base00-rgb-g},${base00-rgb-b}";
    BackgroundAlternate = "${base01-rgb-r},${base01-rgb-g},${base01-rgb-b}";
    DecorationFocus = "${base0D-rgb-r},${base0D-rgb-g},${base0D-rgb-b}";
    DecorationHover = "${base0D-rgb-r},${base0D-rgb-g},${base0D-rgb-b}";
    ForegroundNormal = "${base05-rgb-r},${base05-rgb-g},${base05-rgb-b}";
    ForegroundActive = "${base05-rgb-r},${base05-rgb-g},${base05-rgb-b}";
    ForegroundInactive = "${base05-rgb-r},${base05-rgb-g},${base05-rgb-b}";
    ForegroundLink = "${base05-rgb-r},${base05-rgb-g},${base05-rgb-b}";
    ForegroundVisited = "${base05-rgb-r},${base05-rgb-g},${base05-rgb-b}";
    ForegroundNegative = "${base08-rgb-r},${base08-rgb-g},${base08-rgb-b}";
    ForegroundNeutral = "${base0D-rgb-r},${base0D-rgb-g},${base0D-rgb-b}";
    ForegroundPositive = "${base0B-rgb-r},${base0B-rgb-g},${base0B-rgb-b}";
  };

  colorscheme = {
    General = {
      ColorScheme = colorschemeSlug;
      Name = scheme;
    };

    "ColorEffects:Disabled" = colorEffect;
    "ColorEffects:Inactive" = colorEffect;

    "Colors:Window" = colors;
    "Colors:View" = colors;
    "Colors:Button" = colors;
    "Colors:Tooltip" = colors;
    "Colors:Complementary" = colors;
    "Colors:Selection" = colors // {
      BackgroundNormal = "${base0D-rgb-r},${base0D-rgb-g},${base0D-rgb-b}";
      BackgroundAlternate = "${base0D-rgb-r},${base0D-rgb-g},${base0D-rgb-b}";
      ForegroundNormal = "${base00-rgb-r},${base00-rgb-g},${base00-rgb-b}";
      ForegroundActive = "${base00-rgb-r},${base00-rgb-g},${base00-rgb-b}";
      ForegroundInactive = "${base00-rgb-r},${base00-rgb-g},${base00-rgb-b}";
      ForegroundLink = "${base00-rgb-r},${base00-rgb-g},${base00-rgb-b}";
      ForegroundVisited = "${base00-rgb-r},${base00-rgb-g},${base00-rgb-b}";
    };

    WM = {
      activeBlend = "${base0A-rgb-r},${base0A-rgb-g},${base0A-rgb-b}";
      activeBackground = "${base00-rgb-r},${base00-rgb-g},${base00-rgb-b}";
      activeForeground = "${base05-rgb-r},${base05-rgb-g},${base05-rgb-b}";
      inactiveBlend = "${base03-rgb-r},${base03-rgb-g},${base03-rgb-b}";
      inactiveBackground = "${base00-rgb-r},${base00-rgb-g},${base00-rgb-b}";
      inactiveForeground = "${base05-rgb-r},${base05-rgb-g},${base05-rgb-b}";
    };
  };

  wallpaperMetadata = {
    KPlugin = {
      Id = "stylix";
      Name = "Stylix";
    };
  };

  lookAndFeelMetadata = {
    KPlugin = {
      Id = "stylix";
      Name = "Stylix";
      Description = "Generated from your Home Manager configuration";
      ServiceTypes = [ "Plasma/LookAndFeel" ];
      Website = "https://github.com/danth/stylix";
    };
  };

  lookAndFeelDefaults = {
    kwinrc."org.kde.kdecoration2".library = "org.kde.breeze";
    plasmarc.Theme.name = "default";

    kdeglobals = {
      KDE.widgetStyle = "Breeze";
      General.ColorScheme = colorschemeSlug;
    };

    # This only takes effect on the first login.
    Wallpaper.Image = "stylix";
  };

  # Contains a wallpaper package, a colorscheme file, and a look and feel
  # package which depends on both.
  themePackage = pkgs.runCommandLocal "stylix-kde-theme" {
    colorscheme = formatConfig colorscheme;
    wallpaperMetadata = builtins.toJSON wallpaperMetadata;
    wallpaperImage = config.stylix.image;
    lookAndFeelMetadata = builtins.toJSON lookAndFeelMetadata;
    lookAndFeelDefaults = formatConfig lookAndFeelDefaults;
  } ''
    write_text() {
      mkdir --parents "$(dirname "$2")"
      printf '%s\n' "$1" >"$2"
    }

    PATH="${pkgs.imagemagick}/bin:$PATH"

    wallpaper="$out/share/wallpapers/stylix"
    look_and_feel="$out/share/plasma/look-and-feel/stylix"

    mkdir --parents "$wallpaper/contents/images"

    magick \
      "$wallpaperImage" \
      -thumbnail 400x250 \
      "$wallpaper/contents/screenshot.png"

    dimensions="$(identify -ping -format '%wx%h' "$wallpaperImage")"
    magick "$wallpaperImage" "$wallpaper/contents/images/$dimensions.png"

    write_text \
      "$colorscheme" \
      "$out/share/color-schemes/${colorschemeSlug}.colors"

    write_text "$wallpaperMetadata" "$wallpaper/metadata.json"
    write_text "$lookAndFeelMetadata" "$look_and_feel/metadata.json"
    write_text "$lookAndFeelDefaults" "$look_and_feel/contents/defaults"
  '';

  # The cursor theme can be configured through a look and feel package,
  # however its size cannot.
  kcminputrc = {
    Mouse = {
      cursorSize = makeImmutable (toString config.stylix.cursor.size);
      cursorTheme = makeImmutable config.stylix.cursor.name;
    };
  };

  kded5rc = {
    # The gtkconfig module copies settings from KDE to the GTK configuration.
    # This blocks Home Manager activation because the same files are already
    # managed by Stylix.
    Module-gtkconfig = makeImmutable {
      autoload = false;
    };
  };

  kdeglobals = {
    KDE.LookAndFeelPackage = makeImmutable "stylix";

    General = rec {
      font = makeImmutable "${sansSerif.name},${toString sizes.applications},-1,5,50,0,0,0,0,0";
      fixed = makeImmutable "${monospace.name},${toString sizes.terminal},-1,5,50,0,0,0,0,0";
      desktopFont = makeImmutable "${sansSerif.name},${toString sizes.desktop},-1,5,50,0,0,0,0,0";
      menuFont = desktopFont;
      taskbarFont = desktopFont;
      toolBarFont = desktopFont;
      smallestReadableFont = desktopFont;
    };
  };

  configPackage = pkgs.runCommandLocal "stylix-kde-config" {
    kcminputrc = formatConfig kcminputrc;
    kded5rc = formatConfig kded5rc;
    kdeglobals = formatConfig kdeglobals;
  } ''
    mkdir "$out"

    printf '%s\n' "$kcminputrc" >"$out/kcminputrc"
    printf '%s\n' "$kded5rc" >"$out/kded5rc"
    printf '%s\n' "$kdeglobals" >"$out/kdeglobals"
  '';
    
  envVars = {
    QT_QPA_PLATFORMTHEME = "kde"; 
    QT_STYLE_OVERRIDE = "breeze";
  }; 

in {
  options.stylix.targets.kde.enable =
    config.lib.stylix.mkEnableTarget "KDE" true;

  config = lib.mkIf (config.stylix.enable && config.stylix.targets.kde.enable && pkgs.stdenv.hostPlatform.isLinux) {
    xdg = {
      systemDirs.config = [ "${configPackage}" ];
      configFile."kdeglobals".text = "${formatConfig colorscheme}";
    };

    systemd.user.sessionVariables = envVars;

    qt = {
      enable = true;
    };

    home = {
      packages = with pkgs; [
        themePackage

        # QT6 packages (note that full does not mean "install all of KDE", just all of Qt6)
        (hiPrio kdePackages.full)
        (hiPrio kdePackages.breeze-icons)
        (hiPrio kdePackages.breeze)
        (hiPrio kdePackages.plasma-integration)
        (hiPrio kdePackages.qqc2-breeze-style)
        (hiPrio kdePackages.qqc2-desktop-style)

        # QT5 packages
        libsForQt5.full
        libsForQt5.breeze-icons
        libsForQt5.breeze-qt5
        libsForQt5.qqc2-breeze-style
        libsForQt5.qqc2-desktop-style
        libsForQt5.plasma-integration
      ];

      sessionVariables = envVars;

      # plasma-apply-wallpaperimage is necessary to change the wallpaper
      # after the first login.
      #
      # Home Manager clears $PATH before running the activation script, but we
      # want to avoid installing these tools explicitly because that would pull
      # in large dependencies for people who aren't actually using KDE.
      # The workaround used is to assume a list of common paths where the tools
      # might be installed, and look there. The ideal solution would require
      # changes to KDE to make it possible to update the wallpaper through
      # config files alone.
      activation.stylixLookAndFeel = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        global_path() {
          for directory in /run/current-system/sw/bin /usr/bin /bin; do
            if [[ -f "$directory/$1" ]]; then
              printf '%s\n' "$directory/$1"
              return 0
            fi
          done

          return 1
        }

        if wallpaper_image="$(global_path plasma-apply-wallpaperimage)"; then
          "$wallpaper_image" "${themePackage}/share/wallpapers/stylix"
        else
          verboseEcho \
            "plasma-apply-wallpaperimage: command not found"
        fi
      '';
    };
  };
}
