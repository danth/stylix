{
  pkgs,
  inputs,
  lib,
  testbedFieldSeparator ? ":",
}:

let
  # Creates a minimal configuration to extract the `stylix.testbed.enable`
  # option value.
  #
  # This is for performance reasons. Primarily, to avoid fully evaluating
  # testbed system configurations to determine flake outputs.
  # E.g., when running `nix flake show`.
  isEnabled =
    module:
    let
      minimal = lib.evalModules {
        modules = [
          module
          ./modules/enable.nix
          { _module.check = false; }
          { _module.args = { inherit pkgs; }; }
        ];
      };
    in
    minimal.config.stylix.testbed.enable;

  autoload =
    let
      directory = "testbeds";
      modules = "${inputs.self}/modules";
    in
    lib.pipe modules [
      builtins.readDir
      builtins.attrNames
      (builtins.concatMap (
        module:
        let
          testbeds = "${modules}/${module}/${directory}";
          files = lib.optionalAttrs (builtins.pathExists testbeds) (
            builtins.readDir testbeds
          );
        in
        lib.mapAttrsToList (
          testbed: type:
          if type != "regular" then
            throw "${testbed} must be regular: ${type}"

          else if !lib.hasSuffix ".nix" testbed then
            throw "testbed must be a Nix file: ${testbeds}/${testbed}"

          else if testbed == ".nix" then
            throw "testbed must have a name: ${testbed}"

          else
            {
              inherit module;

              name = lib.removeSuffix ".nix" testbed;
              path = "${testbeds}/${testbed}";
            }
        ) files
      ))
    ];

  makeTestbed =
    testbed: testcase: stylix:
    let
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
            testcase
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

          {
            inherit stylix;
            system.name = name;
          }
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
      ${name} = script;
    };

  # This generates a copy of each testbed for each of the following themes.
  makeTestbeds =
    let
      images = {
        dark = pkgs.fetchurl {
          name = "mountains.jpg";
          url = "https://unsplash.com/photos/ZqLeQDjY6fY/download?ixid=M3wxMjA3fDB8MXxhbGx8fHx8fHx8fHwxNzE2MzY1NDY4fA&force=true";
          hash = "sha256-Dm/0nKiTFOzNtSiARnVg7zM0J1o+EuIdUQ3OAuasM58=";
        };

        light = pkgs.fetchurl {
          name = "three-bicycles.jpg";
          url = "https://unsplash.com/photos/hwLAI5lRhdM/download?ixid=M3wxMjA3fDB8MXxhbGx8fHx8fHx8fHwxNzE2MzYxNDcwfA&force=true";
          hash = "sha256-S0MumuBGJulUekoGI2oZfUa/50Jw0ZzkqDDu1nRkFUA=";
        };
      };
    in
    testbed:
    lib.mapAttrsToList (makeTestbed testbed) {
      light = {
        enable = true;
        image = images.light;
        base16Scheme = "${inputs.tinted-schemes}/base16/catppuccin-latte.yaml";
        polarity = "light";
        cursor = {
          name = "Vanilla-DMZ";
          package = pkgs.vanilla-dmz;
          size = 32;
        };
      };
      dark = {
        enable = true;
        image = images.dark;
        base16Scheme = "${inputs.tinted-schemes}/base16/catppuccin-macchiato.yaml";
        polarity = "dark";
        cursor = {
          name = "Vanilla-DMZ";
          package = pkgs.vanilla-dmz;
          size = 32;
        };
      };
      imageless = {
        enable = true;
        base16Scheme = "${inputs.tinted-schemes}/base16/catppuccin-macchiato.yaml";
        polarity = "dark";
        cursor = {
          name = "Vanilla-DMZ";
          package = pkgs.vanilla-dmz;
          size = 32;
        };
      };
      schemeless = {
        enable = true;
        image = images.dark;
        polarity = "dark";
        cursor = {
          name = "Vanilla-DMZ";
          package = pkgs.vanilla-dmz;
          size = 32;
        };
      };
      cursorless = {
        enable = true;
        image = images.dark;
        base16Scheme = "${inputs.tinted-schemes}/base16/catppuccin-macchiato.yaml";
        polarity = "dark";
      };
    };

in
# Testbeds are merged using lib.attrsets.unionOfDisjoint to throw an error if
# testbed names collide.
builtins.foldl' lib.attrsets.unionOfDisjoint { } (
  builtins.concatMap makeTestbeds autoload
)
