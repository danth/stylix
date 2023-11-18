inputs:
{ config, lib, ... }:

{
  imports = [
    ./utils.nix
    (import ./types.nix inputs)
  ];
}
