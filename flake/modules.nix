{
  inputs,
  self,
  lib,
  ...
}:
{
  flake = {
    nixosModules.stylix =
      { pkgs, ... }@args:
      {
        imports = [
          (import "${self}/stylix/nixos" inputs)
          {
            stylix = {
              inherit inputs;
              paletteGenerator =
                self.packages.${pkgs.stdenv.hostPlatform.system}.palette-generator;
              base16 = inputs.base16.lib args;
              homeManagerIntegration.module = self.homeModules.stylix;
            };
          }
        ];
      };

    # Drop this alias after 26.05
    homeManagerModules = lib.mkIf (lib.oldestSupportedReleaseIsAtLeast 2511) (
      builtins.warn "stylix: flake output `homeManagerModules` has been renamed to `homeModules`" self.homeModules
    );

    homeModules.stylix =
      { pkgs, ... }@args:
      {
        imports = [
          (import "${self}/stylix/hm" inputs)
          {
            stylix = {
              inherit inputs;
              paletteGenerator =
                self.packages.${pkgs.stdenv.hostPlatform.system}.palette-generator;
              base16 = inputs.base16.lib args;
            };
          }
        ];
      };

    darwinModules.stylix =
      { pkgs, ... }@args:
      {
        imports = [
          (import "${self}/stylix/darwin" inputs)
          {
            stylix = {
              inherit inputs;
              paletteGenerator =
                self.packages.${pkgs.stdenv.hostPlatform.system}.palette-generator;
              base16 = inputs.base16.lib args;
              homeManagerIntegration.module = self.homeModules.stylix;
            };
          }
        ];
      };

    nixOnDroidModules.stylix =
      { pkgs, ... }@args:
      {
        imports = [
          (import "${self}/stylix/droid" inputs)
          {
            stylix = {
              paletteGenerator =
                self.packages.${pkgs.stdenv.hostPlatform.system}.palette-generator;
              base16 = inputs.base16.lib args;
              homeManagerIntegration.module = self.homeModules.stylix;
            };
          }
        ];
      };
  };
}
