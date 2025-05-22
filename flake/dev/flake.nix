{
  description = ''
    Private inputs for development purposes. These are used by the top level
    flake in the `dev` partition, but do not appear in consumers' lock files.
  '';

  inputs = {
    # NOTE: Use a different name to the root flake's inputs.nixpkgs to avoid shadowing it.
    # NOTE: The only reason we specify a nixpkgs input at all here, is so the other inputs can follow it.
    # TODO: Once nix 2.26 is more prevalent, follow the root flake's inputs using a "path:../.." input.
    dev-nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    # keep-sorted start block=yes newline_separated=yes
    flake-compat.url = "github:edolstra/flake-compat";

    git-hooks = {
      url = "github:cachix/git-hooks.nix";
      inputs = {
        flake-compat.follows = "flake-compat";
        nixpkgs.follows = "dev-nixpkgs";
      };
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "dev-nixpkgs";
    };
    # keep-sorted end
  };

  # This flake is only used for its inputs.
  outputs = _inputs: { };
}
