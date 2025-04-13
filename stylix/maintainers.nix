# Stylix maintainers.
#
# This attribute set contains Stylix module maintainers that do not have an
# entry in the Nixpkgs maintainer list [1]. Entries here are expected to follow
# the same format as described in [1].
#
# [1]: https://github.com/NixOS/nixpkgs/blob/1da63e6cc622a0cb6fd5b86d49923e4eb1e33b70/maintainers/maintainer-list.nix
{
  butzist = {
    email = "adam@szalkowski.de";
    name = "Adam M. Szalkowski";
    github = "butzist";
    githubId = 2405792;
  };
  cluther = {
    name = "Chet Luther";
    email = "chet.luther@gmail.com";
    github = "cluther";
    githubId = 86579;
  };
  make-42 = {
    email = "ontake@ontake.dev";
    name = "Louis Dalibard";
    matrix = "@ontake:matrix.ontake.dev";
    github = "make-42";
    githubId = 17462236;
    keys = [
      { fingerprint = "36BC 916D DD4E B1EE EE82  4BBF DC95 900F 6DA7 9992"; }
    ];
  };
  skoove = {
    email = "zie@sturges.com.au";
    name = "Zie Sturges";
    github = "skoove";
    githubId = 53106860;
  };
}
