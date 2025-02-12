{
  pkgs,
  config,
  lib,
  ...
}:
let
  cfg = config.stylix.targets.kde;

  inherit (config.lib.stylix)
    colors
    mkEnableTarget
    ;
  inherit (config.stylix)
    image
    ;

  mergeWithImage =
    default: withImage:
    let
      satisfies = check: (check default) && (check withImage);
    in
    # TODO: when adding `wallpaper` option to this module, replace this with `image == null || !cfg.wallpaper`
    if image == null then
      default
    else if satisfies lib.isString then
      default + withImage
    else if satisfies lib.isAttrs then
      default // withImage
    else if satisfies lib.isList then
      default ++ withImage
    else
      throw "unreachable (image merge in stylix KDE module)";

  formatValue =
    value:
    if lib.isBool value then if value then "true" else "false" else toString value;

  formatSection =
    path: data:
    let
      header = lib.concatStrings (map (p: "[${p}]") path);
      formatChild = name: formatLines (path ++ [ name ]);
      children = lib.mapAttrsToList formatChild data;
      partitioned = lib.partition lib.isString children;
      directChildren = partitioned.right;
      indirectChildren = partitioned.wrong;
    in
    lib.optional (directChildren != [ ]) header
    ++ directChildren
    ++ lib.flatten indirectChildren;

  formatLines =
    path: data:
    if lib.isAttrs data then
      if data ? _immutable then
        if lib.isAttrs data.value then
          formatSection (path ++ [ "$i" ]) data.value
        else
          "${lib.last path}[$i]=${formatValue data.value}"
      else
        formatSection path data
    else
      "${lib.last path}=${formatValue data}";

  formatConfig = data: lib.concatStringsSep "\n" (formatLines [ ] data);

  # Marking a setting as immutable should prevent it being overwritten
  # through the system settings menu.
  makeImmutable = value: {
    _immutable = true;
    inherit value;
  };

  # PascalCase is the standard naming for color scheme files. Schemes named
  # in kebab-case will load when selected manually, but don't work with a
  # look and feel package.
  colorschemeSlug = lib.concatStrings (
    lib.filter lib.isString (builtins.split "[^a-zA-Z]" colors.scheme)
  );

  colorEffect = {
    ColorEffect = 0;
    ColorAmount = 0;
    ContrastEffect = 1;
    ContrastAmount = 0.5;
    IntensityEffect = 0;
    IntensityAmount = 0;
  };

  mkColorTriple =
    name:
    lib.concatStringsSep "," (
      map (color: colors."${name}-rgb-${color}") [
        "r"
        "g"
        "b"
      ]
    );

  mkColorMapping =
    num:
    let
      hex = "base0${lib.toHexString num}";
    in
    {
      name = hex;
      value = mkColorTriple hex;
    };

  colors' = lib.listToAttrs (map mkColorMapping (lib.range 0 15));

  kdecolors = with colors'; {
    BackgroundNormal = base00;
    BackgroundAlternate = base01;
    DecorationFocus = base0D;
    DecorationHover = base0D;
    ForegroundNormal = base05;
    ForegroundActive = base05;
    ForegroundInactive = base05;
    ForegroundLink = base05;
    ForegroundVisited = base05;
    ForegroundNegative = base08;
    ForegroundNeutral = base0D;
    ForegroundPositive = base0B;
  };

  colorscheme = {
    General = {
      ColorScheme = colorschemeSlug;
      Name = colors.scheme;
    };

    "ColorEffects:Disabled" = colorEffect;
    "ColorEffects:Inactive" = colorEffect;

    "Colors:Window" = kdecolors;
    "Colors:View" = kdecolors;
    "Colors:Button" = kdecolors;
    "Colors:Tooltip" = kdecolors;
    "Colors:Complementary" = kdecolors;
    "Colors:Selection" =
      kdecolors
      // (with colors'; {
        BackgroundNormal = base0D;
        BackgroundAlternate = base0D;
        ForegroundNormal = base00;
        ForegroundActive = base00;
        ForegroundInactive = base00;
        ForegroundLink = base00;
        ForegroundVisited = base00;
      });

    WM = with colors'; {
      activeBlend = base0A;
      activeBackground = base00;
      activeForeground = base05;
      inactiveBlend = base03;
      inactiveBackground = base00;
      inactiveForeground = base05;
    };
  };

  Id = "stylix";
  Name = "Stylix";

  wallpaperMetadata = {
    KPlugin = {
      inherit Id Name;
    };
  };

  lookAndFeelMetadata = {
    KPlugin = {
      inherit Id Name;
      Description = "Generated from your Home Manager configuration";
      ServiceTypes = [ "Plasma/LookAndFeel" ];
      Website = "https://github.com/danth/stylix";
    };
    KPackageStructure = "Plasma/LookAndFeel";
  };

  lookAndFeelDefaults =
    mergeWithImage
      {
        kwinrc."org.kde.kdecoration2".library = cfg.decorations;
        plasmarc.Theme.name = "default";

        kdeglobals = {
          KDE.widgetStyle = "Breeze";
          General.ColorScheme = colorschemeSlug;
        };
      }
      {
        # This only takes effect on the first login.
        Wallpaper.Image = Id;
      };

  # Contains an optional wallpaper package, a colorscheme file, and a look and feel
  # package which depends on both.
  themePackage =
    pkgs.runCommandLocal "stylix-kde-theme"
      (mergeWithImage
        {
          colorscheme = formatConfig colorscheme;
          lookAndFeelMetadata = builtins.toJSON lookAndFeelMetadata;
          lookAndFeelDefaults = formatConfig lookAndFeelDefaults;
        }
        {
          wallpaperMetadata = builtins.toJSON wallpaperMetadata;
          wallpaperImage = image;
        }
      )
      (
        mergeWithImage
          ''
            write_text() {
              mkdir --parents "$(dirname "$2")"
              printf '%s\n' "$1" >"$2"
            }

            wallpaper="$out/share/wallpapers/${Id}"
            look_and_feel="$out/share/plasma/look-and-feel/${Id}"
            colorschemePath="$out/share/color-schemes/${colorschemeSlug}.colors"

            write_text "$colorscheme" "$colorschemePath"
            write_text "$lookAndFeelMetadata" "$look_and_feel/metadata.json"
            write_text "$lookAndFeelDefaults" "$look_and_feel/contents/defaults"
          ''
          ''
            PATH="${pkgs.imagemagick}/bin:$PATH"

            mkdir --parents "$wallpaper/contents/images"

            magick \
              "$wallpaperImage" \
              -thumbnail 400x250 \
              "$wallpaper/contents/screenshot.png"

            dimensions="$(identify -ping -format '%wx%h' "$wallpaperImage")"
            magick "$wallpaperImage" "$wallpaper/contents/images/$dimensions.png"

            write_text "$wallpaperMetadata" "$wallpaper/metadata.json"
          ''
      );

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
    KDE.LookAndFeelPackage = makeImmutable Id;

    General = with config.stylix.fonts; rec {
      font = makeImmutable "${sansSerif.name},${toString sizes.applications},-1,5,50,0,0,0,0,0";
      fixed = makeImmutable "${monospace.name},${toString sizes.terminal},-1,5,50,0,0,0,0,0";
      desktopFont = makeImmutable "${sansSerif.name},${toString sizes.desktop},-1,5,50,0,0,0,0,0";
      menuFont = desktopFont;
      taskbarFont = desktopFont;
      toolBarFont = desktopFont;
      smallestReadableFont = desktopFont;
    };

    UiSettings.ColorScheme = colorschemeSlug;
  };

  configPackage =
    pkgs.runCommandLocal "stylix-kde-config"
      {
        kcminputrc = formatConfig kcminputrc;
        kded5rc = formatConfig kded5rc;
        kdeglobals = formatConfig kdeglobals;
      }
      ''
        mkdir "$out"

        printf '%s\n' "$kcminputrc" >"$out/kcminputrc"
        printf '%s\n' "$kded5rc" >"$out/kded5rc"
        printf '%s\n' "$kdeglobals" >"$out/kdeglobals"
      '';

  # plasma-apply-wallpaperimage is necessary to change the wallpaper
  # after the first login.
  #
  # plasma-apply-lookandfeel is only here to trigger a hot reload, the theme
  # would still be applied without it if you logged out and back in.
  #
  # Home Manager clears $PATH before running the activation script, but we
  # want to avoid installing these tools explicitly because that would pull
  # in large dependencies for people who aren't actually using KDE.
  # The workaround used is to assume a list of common paths where the tools
  # might be installed, and look there. The ideal solution would require
  # changes to KDE to make it possible to update the wallpaper through
  # config files alone.
  activator' = pkgs.writeShellScriptBin "stylix-activate-kde" (
    mergeWithImage
      ''
        set -eu
        get_exe() {
          for directory in /run/current-system/sw/bin /usr/bin /bin; do
            if [[ -f "$directory/$1" ]]; then
              printf '%s\n' "$directory/$1"
              return 0
            fi
          done
          echo "Skipping '$1': command not found"
          return 1
        }

        if look_and_feel="$(get_exe plasma-apply-lookandfeel)"; then
          "$look_and_feel" --apply "${Id}"
        fi
      ''
      ''
        if wallpaper_image="$(get_exe plasma-apply-wallpaperimage)"; then
          "$wallpaper_image" "${themePackage}/share/wallpapers/${Id}"
        fi
      ''
  );
  activator = lib.getExe activator';
in
{
  options.stylix.targets.kde = {
    enable = mkEnableTarget "KDE" true;

    decorations = lib.mkOption {
      type = lib.types.str;
      default = "org.kde.breeze";
      description = ''
        The library for the window decorations theme.

        Decorations other than default `org.kde.breeze` may not be compatible
        with stylix.

        To list all available decorations, see the `library` key in the
        `org.kde.kdecoration2` section of `$HOME/.config/kwinrc` after
        imperatively applying the window decoration via the System Settings app.
      '';
    };
  };

  config =
    lib.mkIf
      (config.stylix.enable && cfg.enable && pkgs.stdenv.hostPlatform.isLinux)
      {
        home = {
          packages = [ themePackage ];

          # This activation entry will run the theme activator when the homeConfiguration is activated
          activation.stylixLookAndFeel = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
            ${activator} || verboseEcho \
              "Stylix KDE theme setting failed. This only works in a running Plasma session."
          '';
        };

        xdg = {
          systemDirs.config = [ "${configPackage}" ];

          # This desktop entry will run the theme activator when a new Plasma session is started
          # Note: This doesn't run again if a new homeConfiguration is activated from a running Plasma session
          configFile."autostart/stylix-activate-kde.desktop".text = ''
            [Desktop Entry]
            Type=Application
            Exec=${activator}
            Name=Stylix: activate KDE theme
            X-KDE-AutostartScript=true
          '';
        };
      };
}
