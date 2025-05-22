(import (
  let
    inherit (lock.nodes.flake-compat.locked) narHash rev;
    lock = builtins.fromJSON (builtins.readFile ./flake/dev/flake.lock);
  in
  fetchTarball {
    url = "https://github.com/edolstra/flake-compat/archive/${rev}.tar.gz";
    sha256 = narHash;
  }
) { src = ./.; }).defaultNix
