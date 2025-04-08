# Slight modification from nixpkgs's script: https://github.com/NixOS/nixpkgs/commit/4a694fc50007076566a204d6ea623fd5fc7ddbfa
# Copyright (c) 2003-2025 Eelco Dolstra and the Nixpkgs/NixOS contributors

pkgs:
let
  inherit (pkgs.lib)
    add
    attrNames
    attrValues
    elemAt
    foldl'
    genList
    length
    replaceStrings
    sort
    toLower
    trace
    ;

  maintainers = import ./maintainers.nix;
  simplify = replaceStrings [ "-" "_" ] [ "" "" ];
  namesSorted = sort (a: b: a.key < b.key) (
    map (
      n:
      let
        pos = builtins.unsafeGetAttrPos n maintainers;
      in
      assert pos == null -> throw "maintainers entry ${n} is malformed";
      {
        name = n;
        inherit (pos) line;
        key = toLower (simplify n);
      }
    ) (attrNames maintainers)
  );
  before =
    {
      key,
      ...
    }:
    foldl' (
      acc: n: if n.key < key && (acc == null || n.key > acc.key) then n else acc
    ) null namesSorted;
  errors = foldl' add 0 (
    map (
      i:
      let
        a = elemAt namesSorted i;
        b = elemAt namesSorted (i + 1);
        lim =
          let
            t = before a;
          in
          if t == null then "the initial {" else t.name;
      in
      if a.line >= b.line then
        trace (
          "maintainer ${a.name} (line ${toString a.line}) should be listed "
          + "after ${lim}, not after ${b.name} (line ${toString b.line})"
        ) 1
      else
        0
    ) (genList (i: i) (length namesSorted - 1))
  );
in
assert length (attrValues maintainers) < 1 || errors == 0;
pkgs.runCommandLocal "maintainers-sorted" { } "mkdir $out"

# generate edit commands to sort the list.
# may everything following the last current entry (closing } ff) in the wrong place
# with lib;
# concatStringsSep
#   "\n"
#   (let first = foldl' (acc: n: if n.line < acc then n.line else acc) 999999999 namesSorted;
#        commands = map
#          (i: let e = elemAt namesSorted i;
#                  begin = foldl'
#                    (acc: n: if n.line < e.line && n.line > acc then n.line else acc)
#                    1
#                    namesSorted;
#                  end =
#                    foldl' (acc: n: if n.line > e.line && n.line < acc then n.line else acc)
#                      999999999
#                      namesSorted;
#              in "${toString e.line},${toString (end - 1)} p")
#          (genList (i: i) (length namesSorted));
#    in map
#      (c: "sed -ne '${c}' maintainers/maintainer-list.nix")
#      ([ "1,${toString (first - 1)} p" ] ++ commands))
