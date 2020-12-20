{
  outputs = inputs: {
    nixosModules.stylix = import ./default.nix;
  };
}
