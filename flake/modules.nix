{
  inputs,
  self,
  lib,
  ...
}:
{
  flake = {
    # pkgs is specified as an argument even though it isn't used directly in the function body but further down.
    # We need to tell deadnix to skip linting here to prevent it from complaining because of the unused pkgs argument.
    nixosModules.stylix =
      # deadnix: skip
      { pkgs, ... }@args:
      {
        imports = [
          ../stylix/nixos
          {
            stylix = {
              inherit inputs;
              base16 = inputs.base16.lib args;
              homeManagerIntegration.module = self.homeModules.stylix;
            };
          }
        ];
      };

    homeModules.stylix =
      # deadnix: skip
      { pkgs, ... }@args:
      {
        imports = [
          ../stylix/hm
          {
            stylix = {
              inherit inputs;
              base16 = inputs.base16.lib args;
            };
          }
        ];
      };

    darwinModules.stylix =
      # deadnix: skip
      { pkgs, ... }@args:
      {
        imports = [
          ../stylix/darwin
          {
            stylix = {
              inherit inputs;
              base16 = inputs.base16.lib args;
              homeManagerIntegration.module = self.homeModules.stylix;
            };
          }
        ];
      };

    nixOnDroidModules.stylix =
      # deadnix: skip
      { pkgs, ... }@args:
      {
        imports = [
          ../stylix/droid
          {
            stylix = {
              base16 = inputs.base16.lib args;
              homeManagerIntegration.module = self.homeModules.stylix;
            };
          }
        ];
      };
  };
}
