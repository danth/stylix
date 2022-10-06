{ config, pkgs, lib, ... }:

with lib;
with config.lib.stylix.colors;

let
  theme = pkgs.runCommandLocal "plymouth-theme" { } ''
    themeDir="$out/share/plymouth/themes/stylix"
    mkdir -p $themeDir

    # Convert in case the input image is not PNG
    ${pkgs.imagemagick}/bin/convert \
      -background transparent \
      ${config.stylix.targets.plymouth.logo} \
      $themeDir/logo.png

    cp ${config.lib.stylix.pixel "base00"} $themeDir/progress-background.png
    cp ${config.lib.stylix.pixel "base01"} $themeDir/progress-bar.png

    cp ${./theme.script} $themeDir/stylix.script
    substituteInPlace $themeDir/stylix.script \
      --replace "%BASE00%" "${
        if config.stylix.targets.plymouth.blackBackground
        then "0, 0, 0"
        else "${base00-dec-r}, ${base00-dec-g}, ${base00-dec-b}"
      }" \
      --replace "%BASE05%" "${base05-dec-r}, ${base05-dec-g}, ${base05-dec-b}"

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
      description = ''
        Logo to be used on the boot screen.

        This defaults to the NixOS logo, but you could set it to your OEM logo
        if it suits the theme.
      '';
      type = with types; either path package;
      defaultText = literalDocBook "NixOS snowflake";
      default = pkgs.fetchurl {
        url = "https://raw.githubusercontent.com/NixOS/nixos-artwork/master/logo/nix-snowflake.svg";
        sha256 = "SCuQlSPB14GFTq4XvExJ0QEuK2VIbrd5YYKHLRG/q5I=";
      };
    };

    blackBackground = mkOption {
      description = ''
        Whether to use a black background rather than a theme colour.

        This looks good in combination with systemd-boot, as it means that the
        background colour doesn't change throughout the boot process.
      '';
      type = types.bool;
      defaultText = literalDocBook "<literal>true</literal> if systemd-boot is enabled";
      default = config.boot.loader.systemd-boot.enable;
    };
  };

  config.boot.plymouth = mkIf config.stylix.targets.plymouth.enable {
    theme = "stylix";
    themePackages = [ theme ];
  };
}
