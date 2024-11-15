{ pkgs, ... }:

# We are using VSCodium because VSCode is an unfree package
let
  package = pkgs.vscodium;

in
{
  stylix.testbed.application = {
    enable = true;
    name = "codium";
    inherit package;
  };

  home-manager.sharedModules = [
    {
      programs.vscode = {
        enable = true;
        inherit package;
      };
    }
  ];
}
