{ lib, colors, ... }:
color:
let
  r = ((lib.toInt colors."${color}-rgb-r") * 100.0) / 255;
  g = ((lib.toInt colors."${color}-rgb-g") * 100.0) / 255;
  b = ((lib.toInt colors."${color}-rgb-b") * 100.0) / 255;
  max = lib.max r (lib.max g b);
  min = lib.min r (lib.min g b);
  delta = max - min;
  fmod = base: int: base - (int * builtins.floor (base / int));
  h =
    if delta == 0 then
      0
    else if max == r then
      60 * (fmod ((g - b) / delta) 6)
    else if max == g then
      60 * (((b - r) / delta) + 2)
    else if max == b then
      60 * (((r - g) / delta) + 4)
    else
      0;
  l = (max + min) / 2;
  s =
    if delta == 0 then
      0
    else
      100 * delta / (100 - lib.max (2 * l - 100) (100 - (2 * l)));
  roundToString = value: toString (builtins.floor (value + 0.5));
in
lib.concatMapStringsSep " " roundToString [
  h
  s
  l
]
