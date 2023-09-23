{ palette-generator, base16 }:
{ config, lib, ... }:

{
  imports = [
    ./utils.nix
    (import ./types.nix { inherit base16; })
    (import ./constructors.nix { inherit palette-generator base16; })
  ];
}
