{ pkgs, lib, config, ... }:

let
  cfg = config.stylix.targets.iconTheme;
  basic-colormath = pkgs.python3.pkgs.buildPythonPackage rec {
    pname = "basic-colormath";
    version = "0.5.0";
    pyproject = true;

    src = pkgs.fetchPypi {
      inherit version;
      pname = "basic_colormath";
      hash = "sha256-p/uNuNg5kqKIkeMmX5sWY8umGAg0E4/otgQxhzIuo0E=";
    };

    propagatedBuildInputs = with pkgs.python3.pkgs; [
      setuptools
      setuptools-scm
      pillow
    ];
  };
  pythonEnv = pkgs.python3.withPackages (
    ps: with ps; [
      basic-colormath
      colormath
      tqdm
      pillow
    ]
  );
  recolorScript = with cfg.recolor; ''
    ${pythonEnv}/bin/python ${./recolor.py} --src $out/share/icons \
      --smooth '${toString smooth}' \
      ${if cfg.recolor.mode == "monochrome" then
        "--monochrome '${builtins.concatStringsSep "," cfg.recolor.colors}'"
      else
        "--palette ''${builtins.concatStringsSep "," cfg.recolor.colors}''"}
  '';
in
{
  options.stylix.targets.iconTheme = {
    enable = config.lib.stylix.mkEnableTarget "the icon theme" true;
    name = lib.mkOption {
      description = "The icon theme name";
      type = lib.types.str;
      default = "Zafiro-icons-Dark";
    };
    package = lib.mkOption {
      description = "The icon theme package";
      type = lib.types.package;
      default = pkgs.zafiro-icons;
    };
    recolor = {
      enable = lib.mkOption {
        description = "Whether to recolor the icon theme colors.";
        type = lib.types.bool;
        default = true;
      };
      enableForParentThemes = lib.mkOption {
        description = "Whether to recolor the parent icon theme colors.";
        type = lib.types.bool;
        default = true;
      };
      mode = lib.mkOption {
        description = ''The mode to use:
          monochrome = A monochromatic variant, colored by appropriate shades of the provided base color.
          palette = A multichromatic variant, where all colors are replaced by their nearest perceived equivalent that adheres to the provided color palette.
        '';
        type = lib.types.enum [ "monochrome" "palette" ];
        default = "palette";
      };
      smooth = lib.mkOption {
        description = "#TODO explain this? kinda hard, showing some pictures will be easier.";
        type = lib.types.bool;
        default = true;
      };
      colors = lib.mkOption {
        description = "The color list";
        type = lib.types.listOf (lib.types.str);
        default =  with config.lib.stylix.colors.withHashtag; [
          base08
          base09
          base0A
          base0B
          base0C
          base0D
          base0E
          base0F
        ];
      };
    };
  };

  config = lib.mkIf cfg.enable {
    gtk = {
      enable = true;
      iconTheme = lib.mkMerge [
        {
          name = cfg.name;
        }

        (lib.mkIf (!cfg.recolor.enable) {
          package = cfg.package;
        })
        (lib.mkIf (cfg.recolor.enable) {
          package = cfg.package.overrideAttrs
            (oldAttrs: rec {
              propagatedBuildInputs =
                if cfg.recolor.enableForParentThemes then
                  builtins.map
                    (parentIconTheme: parentIconTheme.overrideAttrs (parentIconThemeOldAttrs: rec {
                      postInstall = recolorScript + (parentIconThemeOldAttrs.postInstall or "");
                    }))
                    # For some reason patching gnome-icon-theme takes forever
                    (builtins.filter (parentIconTheme: parentIconTheme.pname != "gnome-icon-theme") oldAttrs.propagatedBuildInputs)
                else oldAttrs.propagatedBuildInputs;

              postInstall = recolorScript + (oldAttrs.postInstall or "");
            });
        })
      ];
    };
  };
}
