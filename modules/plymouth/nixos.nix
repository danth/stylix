{ config, pkgs, lib, ... }:

with lib;
with config.lib.stylix.colors;

let
  cfg = config.stylix.targets.plymouth;

  theme = pkgs.runCommand "stylix-plymouth" { } ''
    themeDir="$out/share/plymouth/themes/stylix"
    mkdir -p $themeDir

    ${pkgs.imagemagick}/bin/convert \
      -background transparent \
      -bordercolor transparent \
      ${
        # A transparent border ensures the image is not clipped when rotated
        optionalString cfg.logoAnimated "-border 42%"
      } \
      ${cfg.logo} \
      $themeDir/logo.png

    ${
      if cfg.logoAnimated
      then "cp ${./theme.script} $themeDir/stylix.script"
      else "cp ${./theme_still.script} $themeDir/stylix.script"
    }

    ${
      if cfg.blackBackground
      then ''
        substituteInPlace $themeDir/stylix.script \
          --replace "%BASE00%" "0, 0, 0" \
          --replace "%BASE05%" "1, 1, 1"
      ''
      else ''
        substituteInPlace $themeDir/stylix.script \
          --replace "%BASE00%" "${base00-dec-r}, ${base00-dec-g}, ${base00-dec-b}" \
          --replace "%BASE05%" "${base05-dec-r}, ${base05-dec-g}, ${base05-dec-b}"
      ''
    }

    echo "
    [Plymouth Theme]
    Name=Stylix
    ModuleName=script

    [script]
    ImageDir=$themeDir
    ScriptFile=$themeDir/stylix.script
    " > $themeDir/stylix.plymouth
  '';

in {
  options.stylix.targets.plymouth = {
    enable = config.lib.stylix.mkEnableTarget "the Plymouth boot screen" true;

    logo = mkOption {
      description = mdDoc "Logo to be used on the boot screen.";
      type = with types; either path package;
      defaultText = literalMD "NixOS logo";
      default = pkgs.fetchurl {
        url = "https://raw.githubusercontent.com/NixOS/nixos-artwork/f84c13adae08e860a7c3f76ab3a9bef916d276cc/logo/nix-snowflake-colours.svg";
        sha256 = "pHYa+d5f6MAaY8xWd3lDjhagS+nvwDL3w7zSsQyqH7A=";
      };
    };

    logoAnimated = mkOption {
      description = mdDoc ''
        Whether to apply a spinning animation to the logo.

        Disabling this allows the use of logos which don't have rotational
        symmetry.
      '';
      type = types.bool;
      default = true;
    };

    blackBackground = mkOption {
      description = mdDoc ''
        Whether to use a black background rather than a theme colour.

        This looks good in combination with systemd-boot, as it means that the
        background colour doesn't change throughout the boot process.
      '';
      type = types.bool;
      defaultText = literalMD "`true` if systemd-boot is enabled";
      default = config.boot.loader.systemd-boot.enable;
    };
  };

  config.boot.plymouth = mkIf cfg.enable {
    theme = "stylix";
    themePackages = [ theme ];
  };
}
