{ palette-generator, base16 }:
{ config, lib, ... }:

{
  imports = [
      ./utils.nix
      ./types.nix
      (import ./constructors.nix { inherit palette-generator base16; })
  ];
}
