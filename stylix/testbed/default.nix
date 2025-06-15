{
  pkgs,
  inputs,
  lib,
  testbedFieldSeparator ? ":",
}:

let
  isEnabled = pkgs.callPackage ./is-enabled.nix { };

  autoload = lib.pipe ../../modules [
    builtins.readDir
    builtins.attrNames
    (builtins.concatMap (
      module:
      let
        testbeds = ../../modules/${module}/testbeds;
        files = lib.optionalAttrs (builtins.pathExists testbeds) (
          builtins.readDir testbeds
        );
      in
      lib.mapAttrsToList (
        testbed: type:
        let
          path = testbeds + "/${testbed}";
          pathStr = toString path;
        in
        if type != "regular" then
          throw "${pathStr} must be regular: ${type}"

        else if !lib.hasSuffix ".nix" testbed then
          throw "testbed must be a Nix file: ${pathStr}"

        else if testbed == ".nix" then
          throw "testbed must have a name: ${pathStr}"

        else
          {
            inherit module path;
            name = lib.removeSuffix ".nix" testbed;
          }
      ) files
    ))
  ];

  makeTestbed =
    testbed: themeName: themeModule:
    let
      meta = pkgs.callPackage ../meta.nix { };

      name =
        lib.concatMapStringsSep testbedFieldSeparator
          (
            field:
            lib.throwIf (lib.hasInfix testbedFieldSeparator field)
              "testbed field must not contain the '${testbedFieldSeparator}' testbed field separator: ${field}"
              field
          )
          [
            "testbed"
            testbed.name
            themeName
          ];

      system = lib.nixosSystem {
        inherit (pkgs) system;

        modules = [
          ./modules/common.nix
          ./modules/enable.nix
          ./modules/application.nix
          inputs.self.nixosModules.stylix
          inputs.home-manager.nixosModules.home-manager
          testbed.path
          themeModule
          { system.name = name; }
        ];
      };

      script = pkgs.writeShellApplication {
        inherit name;
        text = ''
          cleanup() {
            if rm --recursive "$directory"; then
              printf '%s\n' 'Virtualisation disk image removed.'
            fi
          }

          # We create a temporary directory rather than a temporary file, since
          # temporary files are created empty and are not valid disk images.
          directory="$(mktemp --directory)"
          trap cleanup EXIT

          NIX_DISK_IMAGE="$directory/nixos.qcow2" \
            ${lib.getExe system.config.system.build.vm}
        '';
      };
    in
    lib.optionalAttrs (isEnabled testbed.path) {
      ${name} = lib.recursiveUpdate script {
        meta.maintainers = meta.${testbed.module}.maintainers;
      };
    };

  # Import all the testbed themes
  themes = lib.pipe ./themes [
    builtins.readDir
    (lib.filterAttrs (name: _: lib.strings.hasSuffix ".nix" name))
    (builtins.mapAttrs (
      name: type:
      lib.throwIfNot (type == "regular")
        "Unexpected filetype in testbed themes: ${toString ./themes/${name}} is a ${type}."
        ./themes/${name}
    ))
    (lib.mapAttrs' (name: lib.nameValuePair (lib.strings.removeSuffix ".nix" name)))
  ];

  # This generates a copy of each testbed for each of the imported themes.
  makeTestbeds = testbed: lib.mapAttrsToList (makeTestbed testbed) themes;

in
# Testbeds are merged using lib.attrsets.unionOfDisjoint to throw an error if
# testbed names collide.
builtins.foldl' lib.attrsets.unionOfDisjoint { } (
  builtins.concatMap makeTestbeds autoload
)
