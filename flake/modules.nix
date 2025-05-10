{ inputs, self, ... }:
{
  flake = {
    nixosModules.stylix =
      { pkgs, ... }@args:
      {
        imports = [
          (import ../stylix/nixos inputs)
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

    homeManagerModules = builtins.warn "stylix: flake output homeManagerModules is deprecated, please use homeModules" self.homeModules;

    homeModules.stylix =
      { pkgs, ... }@args:
      {
        imports = [
          (import ../stylix/hm inputs)
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
          (import ../stylix/darwin inputs)
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
          (import ../stylix/droid inputs)
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
