inputs:
{
  palette-generator,
  base16,
  homeManagerModule,
}:
{ lib, ... }:

let
  autoload = import ../autoload.nix { inherit lib inputs; } "droid";
in
{
  imports = [
    ../pixel.nix
    ../target.nix
    ../opacity.nix
    ./fonts.nix
    ./terminal.nix
    (import ./palette.nix { inherit palette-generator base16; })
    (import ../templates.nix inputs)
    (import ../home-manager-integration.nix homeManagerModule)
  ] ++ autoload;

  # See https://github.com/nix-community/nix-on-droid/issues/436
  options.lib = lib.mkOption {
    type = with lib.types; attrsOf attrs;
  };
}
