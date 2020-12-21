{ config, pkgs, ... }:

with config.lib.stylix.colors;

let theme = pkgs.runCommandLocal "plymouth-theme" {}
  ''
    themeDir="$out/share/plymouth/themes/stylix"
    mkdir -p $themeDir

    cp ${./theme.script} $themeDir/stylix.script

    # Convert in case the input image is not PNG
    ${pkgs.imagemagick}/bin/convert ${config.stylix.image} $themeDir/background.png

    # A single pixel of base0B, will be stretched to make the progress bar
    # (Plymouth scripts can only display images)
    ${pkgs.imagemagick}/bin/convert xc:#${base0B-hex} $themeDir/progress.png

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
  boot.plymouth = {
    theme = "stylix";
    themePackages = [ theme ];
  };
}
