{
  pkgs,
  lib,
  inputs,
  ...
}:

let
  nixosConfiguration = lib.nixosSystem {
    inherit (pkgs) system;
    modules = [
      inputs.home-manager.nixosModules.home-manager
      inputs.self.nixosModules.stylix
      ./settings.nix
    ];
  };

  homeManagerConfiguration = inputs.home-manager.lib.homeManagerConfiguration {
    inherit pkgs;
    modules = [
      inputs.self.homeManagerModules.stylix
      ./settings.nix
      {
        home = {
          homeDirectory = "/home/book";
          stateVersion = "22.11";
          username = "book";
        };
      }
    ];
  };

  # TODO: Include Nix Darwin options

  makeOptionsDoc =
    { configuration, pathFilter }:
    (pkgs.nixosOptionsDoc {
      inherit (configuration) options;
      transformOptions =
        option:
        option
        // {
          visible = option.visible && lib.any pathFilter option.declarations;
        };
    }).optionsCommonMark;

  # The documentation for options which aren't linked to a specific module
  makeGlobalOptionsDoc =
    configuration:
    makeOptionsDoc {
      inherit configuration;
      pathFilter =
        path:
        lib.hasPrefix "${inputs.self}/" path
        && !lib.hasPrefix "${inputs.self}/modules/" path;
    };

  # Returns an attribute set of module names and their corresponding option
  # documentation.
  makeModuleOptionsDoc =
    configuration:
    lib.mapAttrs (
      module: _:
      makeOptionsDoc {
        inherit configuration;
        pathFilter = lib.hasPrefix "${inputs.self}/modules/${module}/";
      }
    ) (builtins.readDir "${inputs.self}/modules");

  nixosModuleOptionsDoc = makeModuleOptionsDoc nixosConfiguration;
  homeManagerModuleOptionsDoc = makeModuleOptionsDoc homeManagerConfiguration;

  modulePageScript = lib.pipe "${inputs.self}/modules" [
    builtins.readDir
    (lib.mapAttrsToList (
      module: _: ''
        writeModulePage \
          ${module} \
          ${homeManagerModuleOptionsDoc.${module}} \
          ${nixosModuleOptionsDoc.${module}}
      ''
    ))
    lib.concatStrings
  ];

in
pkgs.stdenvNoCC.mkDerivation {
  name = "stylix-book";
  src = ./.;
  buildInputs = with pkgs; [
    mdbook
    mdbook-alerts
  ];

  patchPhase = ''
    # The generated documentation has headings at level 2, but we want level 3
    # so they can be nested under the sections for each module system.
    REDUCE_HEADINGS='s/^## /### /'

    # The "declared by" links point to a file which only exists when the docs
    # are built locally. This removes the links.
    REMOVE_DECLARED_BY='/*Declared by:*/,/^$/d'

    function writeOptions() {
      platformName="$1"
      optionsFile="$2"
      outputFile="$3"

      echo "## $platformName options" >>"$outputFile"

      if [ -s "$optionsFile" ]; then
        sed -e "$REDUCE_HEADINGS" -e "$REMOVE_DECLARED_BY" <"$optionsFile" >>"$outputFile"
      else
        echo '*None provided.*' >>"$outputFile"
      fi
    }

    function writeModulePage() {
      moduleName="$1"
      homeManagerOptionsFile="$2"
      nixosOptionsFile="$3"

      page="options/modules/$moduleName.md"
      outputFile="src/$page"

      if [ ! -f "$file" ]; then
        echo "# $moduleName" >>"$outputFile"
        echo ${lib.escapeShellArg ''
          > [!NOTE]
          >
          > This module doesn't include any additional documentation.
          > You can browse the options it provides below.
        ''} >>"$outputFile"
      fi

      writeOptions 'Home Manager' "$homeManagerOptionsFile" "$outputFile"
      writeOptions 'NixOS' "$nixosOptionsFile" "$outputFile"

      echo "  - [$moduleName]($page)" >>src/SUMMARY.md
    }

    cp ${../README.md} src/README.md
    cp ${../gnome.png} src/gnome.png
    cp ${../kde.png} src/kde.png

    mkdir -p src/options/global
    writeOptions 'Home Manager' ${(makeGlobalOptionsDoc homeManagerConfiguration)} src/options/global/home_manager.md
    writeOptions 'NixOS' ${(makeGlobalOptionsDoc nixosConfiguration)} src/options/global/nixos.md

    mkdir -p src/options/modules
    ${modulePageScript}
  '';

  buildPhase = "mdbook build --dest-dir $out";
}
