import json
import sys
import colorgram
import colorsys


# Select 9 colors from the image passed on the command line
colors = colorgram.extract(sys.argv[1], 9)


# Extract the most dominant color to use as the background
colors.sort(key=lambda c: c.proportion)
dominant_color = colors.pop(0)

# Decide whether to generate a light or dark scheme based
# on the lightness of the dominant color
if dominant_color.hsl.l >= 128:
    def scale(i):
        scale = 0.7 - (0.1 * i)
        return scale * 255

    def clamp(l):
        return min(l, 100)
else:
    def scale(i):
        scale = 0.1 + (0.1 * i)
        return scale * 255

    def clamp(l):
        return max(l, 155)


def int_to_base(i):
    return "base{0:02X}".format(i)

scheme = {}

# base00 to base07 use the dominant color's hue and saturation,
# lightness is a linear scale
for i in range(8):
    scheme[int_to_base(i)] = (
        dominant_color.hsl.h,
        scale(i),
        dominant_color.hsl.s,
    )

# base08 to base0A use the remaining 8 colors from the image,
# with their lightness clamped to enforce adequate contrast
colors.sort(key=lambda c: c.hsl.h)  # sort by hue
for i in range(8, 16):
    color = colors[i-8]
    scheme[int_to_base(i)] = (
        color.hsl.h,
        clamp(color.hsl.l),
        color.hsl.s,
    )

# Override with any manually selected colors
manual_colors = json.load(sys.stdin)
for k, v in manual_colors.items():
    if v is not None:
      scheme[k] = colorsys.rgb_to_hls(
          int(v[0:2], 16) / 255,
          int(v[2:4], 16) / 255,
          int(v[4:6], 16) / 255,
      )
      scheme[k] = (
          scheme[k][0] * 255,
          scheme[k][1] * 255,
          scheme[k][2] * 255,
      )


data = {}

for key, color in scheme.items():
    r, g, b = colorsys.hls_to_rgb(
        color[0] / 255,
        color[1] / 255,
        color[2] / 255,
    )
    data[key + "-dec-r"] = r
    data[key + "-dec-g"] = g
    data[key + "-dec-b"] = b
    data[key + "-rgb-r"] = r * 255
    data[key + "-rgb-g"] = g * 255
    data[key + "-rgb-b"] = b * 255

    hex_color = "{0:02x}{1:02x}{2:02x}".format(
        round(r * 255),
        round(g * 255),
        round(b * 255),
    )
    data[key + "-hex"] = hex_color
    data[key + "-hash"] = "#" + hex_color
    data[key + "-hex-r"] = hex_color[0:2]
    data[key + "-hex-g"] = hex_color[2:4]
    data[key + "-hex-b"] = hex_color[4:6]

json.dump(data, sys.stdout)
