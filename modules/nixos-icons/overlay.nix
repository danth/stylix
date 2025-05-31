{
  name = "nixos-icons";
  humanName = "the NixOS logo";
  addEnableOption = true;

  overlay =
    { colors }:
    pkgs: super: {
      nixos-icons = super.nixos-icons.overrideAttrs (oldAttrs: {
        src = pkgs.applyPatches {
          inherit (oldAttrs) src;
          prePatch = with colors; ''
            substituteInPlace logo/nix-snowflake-white.svg --replace-fail '#ffffff' '#${base05}'

            # Insert attribution comment after the XML prolog
            sed \
              --in-place \
              '2i<!-- The original NixOS logo from ${oldAttrs.src.url} is licensed under https://creativecommons.org/licenses/by/4.0 and has been modified to match the ${scheme} color scheme. -->' \
              logo/nix-snowflake-white.svg
          '';
        };
      });
    };
}
