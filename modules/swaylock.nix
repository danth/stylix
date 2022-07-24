{ config, lib, ... }:

with config.lib.stylix.colors;

let
  inside = base01-hex;
  ring = base05-hex;
  text = base05-hex;
  positive = base0B-hex;
  negative = base08-hex;

in {
  # mkAfter is important for those who use nixpkgs-wayland,
  # as otherwise it could overwrite our wrapper
  nixpkgs.overlays = lib.mkAfter [
    (final: prev: {
      swaylock = final.writeShellScriptBin "swaylock" ''
        exec ${prev.swaylock}/bin/swaylock \
          --image ${config.stylix.image} \
          --scaling fill \
          --inside-color ${inside} \
          --inside-clear-color ${inside} \
          --inside-caps-lock-color ${inside} \
          --inside-ver-color ${inside} \
          --inside-wrong-color ${inside} \
          --key-hl-color ${positive} \
          --layout-bg-color ${inside} \
          --layout-border-color ${ring} \
          --layout-text-color ${text} \
          --line-uses-inside \
          --ring-color ${ring} \
          --ring-clear-color ${negative} \
          --ring-caps-lock-color ${ring} \
          --ring-ver-color ${positive} \
          --ring-wrong-color ${negative} \
          --separator-color 00000000 \
          --text-color ${text} \
          --text-clear-color ${text} \
          --text-caps-lock-color ${text} \
          --text-ver-color ${text} \
          --text-wrong-color ${text} \
          "$@"
      '';
    })
  ];
}
