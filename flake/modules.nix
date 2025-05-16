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
          (lib.modules.importApply "${self}/stylix/nixos" inputs)
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

    homeModules.stylix =
      { pkgs, ... }@args:
      {
        imports = [
          (lib.modules.importApply "${self}/stylix/hm" inputs)
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
          (lib.modules.importApply "${self}/stylix/darwin" inputs)
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
          (lib.modules.importApply "${self}/stylix/droid" inputs)
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
