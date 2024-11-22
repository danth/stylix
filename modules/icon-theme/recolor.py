# pylint: skip-file
# pylint: disable=W,C,R
# pylint: disable-all
# type: ignore

# Desc: A program for recoloring icon packs, themes and wallpapers. For NovaOS.
# Auth: Nicklas Vraa

from typing import Annotated,List, Set, Tuple, Dict, Optional
from tqdm import tqdm
# from basic_colormath.type_hints import RGB, Lab
from basic_colormath.distance import rgb_to_lab, get_delta_e_lab
from PIL import Image, ImageDraw # noqa
import os, re, shutil, json, subprocess, argparse, random # noqa

# Using custom type hints as the default ones in basic_colormath.type_hits arent compatible past python 3.8
RGB = Annotated[tuple[float, float, float], ([0, 255], [0, 255], [0, 255])]
Lab = Annotated[tuple[float, float, float], ([0, 100], [-128, 127], [-128, 127])]
def LabColor(L:float, a:float, b:float) -> Lab:
    return L, a, b
def sRGBColor(r:float, g:float, b:float) -> RGB:
    return r,g,b

# Global constants -------------------------------------------------------------

# A dynamic dictionary to avoid multiple color conversions.
hex_to_lab_dict = {
    "#ffffff": LabColor(9341.568974319263, -0.037058350415009045, -0.6906417562959177), # White.
    "#000000": LabColor(0,0,0) # Black.
}

name_to_hex_dict = {
    "aliceblue": "#f0f8ff",
    "antiquewhite": "#faebd7",
    "aqua": "#00ffff",
    "aquamarine": "#7fffd4",
    "azure": "#f0ffff",
    "beige": "#f5f5dc",
    "bisque": "#ffe4c4",
    "black": "#000000",
    "blanchedalmond": "#ffebcd",
    "blue": "#0000ff",
    "blueviolet": "#8a2be2",
    "brown": "#a52a2a",
    "burlywood": "#deb887",
    "cadetblue": "#5f9ea0",
    "chartreuse": "#7fff00",
    "chocolate": "#d2691e",
    "coral": "#ff7f50",
    "cornflowerblue": "#6495ed",
    "cornsilk": "#fff8dc",
    "crimson": "#dc143c",
    "cyan": "#00ffff",
    "darkblue": "#00008b",
    "darkcyan": "#008b8b",
    "darkgoldenrod": "#b8860b",
    "darkgray": "#a9a9a9",
    "darkgreen": "#006400",
    "darkgrey": "#a9a9a9",
    "darkkhaki": "#bdb76b",
    "darkmagenta": "#8b008b",
    "darkolivegreen": "#556b2f",
    "darkorange": "#ff8c00",
    "darkorchid": "#9932cc",
    "darkred": "#8b0000",
    "darksalmon": "#e9967a",
    "darkseagreen": "#8fbc8f",
    "darkslateblue": "#483d8b",
    "darkslategray": "#2f4f4f",
    "darkslategrey": "#2f4f4f",
    "darkturquoise": "#00ced1",
    "darkviolet": "#9400d3",
    "deeppink": "#ff1493",
    "deepskyblue": "#00bfff",
    "dimgray": "#696969",
    "dimgrey": "#696969",
    "dodgerblue": "#1e90ff",
    "firebrick": "#b22222",
    "floralwhite": "#fffaf0",
    "forestgreen": "#228b22",
    "fuchsia": "#ff00ff",
    "gainsboro": "#dcdcdc",
    "ghostwhite": "#f8f8ff",
    "goldenrod": "#daa520",
    "gold": "#ffd700",
    "gray": "#808080",
    "green": "#008000",
    "greenyellow": "#adff2f",
    "grey": "#808080",
    "honeydew": "#f0fff0",
    "hotpink": "#ff69b4",
    "indianred": "#cd5c5c",
    "indigo": "#4b0082",
    "ivory": "#fffff0",
    "khaki": "#f0e68c",
    "lavenderblush": "#fff0f5",
    "lavender": "#e6e6fa",
    "lawngreen": "#7cfc00",
    "lemonchiffon": "#fffacd",
    "lightblue": "#add8e6",
    "lightcoral": "#f08080",
    "lightcyan": "#e0ffff",
    "lightgoldenrodyellow": "#fafad2",
    "lightgray": "#d3d3d3",
    "lightgreen": "#90ee90",
    "lightgrey": "#d3d3d3",
    "lightpink": "#ffb6c1",
    "lightsalmon": "#ffa07a",
    "lightseagreen": "#20b2aa",
    "lightskyblue": "#87cefa",
    "lightslategray": "#778899",
    "lightslategrey": "#778899",
    "lightsteelblue": "#b0c4de",
    "lightyellow": "#ffffe0",
    "lime": "#00ff00",
    "limegreen": "#32cd32",
    "linen": "#faf0e6",
    "magenta": "#ff00ff",
    "maroon": "#800000",
    "mediumaquamarine": "#66cdaa",
    "mediumblue": "#0000cd",
    "mediumorchid": "#ba55d3",
    "mediumpurple": "#9370db",
    "mediumseagreen": "#3cb371",
    "mediumslateblue": "#7b68ee",
    "mediumspringgreen": "#00fa9a",
    "mediumturquoise": "#48d1cc",
    "mediumvioletred": "#c71585",
    "midnightblue": "#191970",
    "mintcream": "#f5fffa",
    "mistyrose": "#ffe4e1",
    "moccasin": "#ffe4b5",
    "navajowhite": "#ffdead",
    "navy": "#000080",
    "oldlace": "#fdf5e6",
    "olive": "#808000",
    "olivedrab": "#6b8e23",
    "orange": "#ffa500",
    "orangered": "#ff4500",
    "orchid": "#da70d6",
    "palegoldenrod": "#eee8aa",
    "palegreen": "#98fb98",
    "paleturquoise": "#afeeee",
    "palevioletred": "#db7093",
    "papayawhip": "#ffefd5",
    "peachpuff": "#ffdab9",
    "peru": "#cd853f",
    "pink": "#ffc0cb",
    "plum": "#dda0dd",
    "powderblue": "#b0e0e6",
    "purple": "#800080",
    "rebeccapurple": "#663399",
    "red": "#ff0000",
    "rosybrown": "#bc8f8f",
    "royalblue": "#4169e1",
    "saddlebrown": "#8b4513",
    "salmon": "#fa8072",
    "sandybrown": "#f4a460",
    "seagreen": "#2e8b57",
    "seashell": "#fff5ee",
    "sienna": "#a0522d",
    "silver": "#c0c0c0",
    "skyblue": "#87ceeb",
    "slateblue": "#6a5acd",
    "slategray": "#708090",
    "slategrey": "#708090",
    "snow": "#fffafa",
    "springgreen": "#00ff7f",
    "steelblue": "#4682b4",
    "tan": "#d2b48c",
    "teal": "#008080",
    "thistle": "#d8bfd8",
    "tomato": "#ff6347",
    "turquoise": "#40e0d0",
    "violet": "#ee82ee",
    "wheat": "#f5deb3",
    "white": "#ffffff",
    "whitesmoke": "#f5f5f5",
    "yellow": "#ffff00",
    "yellowgreen": "#9acd32"
}

# Basic utility ----------------------------------------------------------------

def expand_path(path:str) -> str:
    """ Returns the absolute version of the given path, and expands unix notation like tilde for home folder. """
    return os.path.abspath(os.path.expanduser(path))

def is_empty(list:List) -> bool:
    """ Returns true if given list is empty, else false. """
    return len(list) == 0

def load_json_file(path:str) -> Dict:
    """ Return object defined in given json file. """
    with open(path, 'r') as file:
        obj = json.load(file)
    return obj

def check_path(path:str) -> None:
    if not os.path.exists(expand_path(path)):
        raise Exception("Invalid path: " + path)

def svg_to_png(src_path:str, dest_path:str, width:int = 300) -> None:
    """ Generate pngs at given destination path from a given source folder with a given width. """

    src_path = expand_path(src_path)
    dest_path = expand_path(dest_path)

    try:
        subprocess.run(['inkscape', '--version'], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    except FileNotFoundError:
        raise RuntimeError("Inkscape is not installed.")

    os.makedirs(dest_path, exist_ok=True)
    svgs = [file for file in os.listdir(src_path) if file.endswith('.svg')]

    for svg in svgs:
        svg_path = os.path.join(src_path, svg)
        png = os.path.splitext(svg)[0] + '.png'
        png_path = os.path.join(dest_path, png)
        command = ['inkscape', svg_path, '-o', png_path, '-w', str(width)]
        subprocess.run(command)

# Color conversion -------------------------------------------------------------

def hex_to_rgb(hex:str) -> Tuple[int,int,int]:
    """ Converts 6-digit hexadecimal color to rgb color. """
    return int(hex[1:3], 16), int(hex[3:5], 16), int(hex[5:7], 16)

def hex_to_hsl(hex:str) -> Tuple[float,float,float]:
    """ Converts 6-digit hexadecimal color to hsl color. """
    return rgb_to_hsl(hex_to_rgb(hex))

def hex_to_gray(hex:str) -> str:
    """ Grayscales a given 6-digit hexadecimal color. """
    r, g, b = hex_to_rgb(hex)
    return '#' + format(int(0.21*r + 0.72*g + 0.07*b), '02x')*3

def rgb_to_hex(rgb:Tuple[int,int,int]) -> str:
    """ Converts rgb color to 6-digit hexadecimal color. """
    r, g, b = rgb
    return "#{:02x}{:02x}{:02x}".format(r, g, b)

def rgba_to_hex(rgba:Tuple[int,int,int,Optional[float]]) -> str:
    """ Converts rgba color to 8-digit hexadecimal color. """
    r, g, b = rgba[:3]
    hex = "#{:02X}{:02X}{:02X}".format(r, g, b)

    if len(rgba) > 3:
        if rgba[3] != 1.0:
            hex += format(int(rgba[3] * 255), '02X')

    return hex

def rgb_to_hsl(rgb:Tuple[int,int,int]) -> Tuple[float,float,float]:
    """ Converts rgb color to hsl color. """
    r, g, b = rgb
    r /= 255.0; g /= 255.0; b /= 255.0
    max_val = max(r, g, b); min_val = min(r, g, b)
    h = s = l = (max_val + min_val) / 2.0

    if max_val == min_val:
        h = s = 0
    else:
        d = max_val - min_val
        s = d / (2.0 - max_val - min_val)

        if max_val == r: h = (g - b) / d + (6.0 if g < b else 0.0)
        elif max_val == g: h = (b - r) / d + 2.0
        else: h = (r - g) / d + 4.0
        h /= 6.0

    return h, s, l

def rgb_to_gray(rgb:Tuple[int,int,int]) -> Tuple[int,int,int]:
    """ Grayscales a given rgb color. """
    r, g, b = rgb
    weighed_avg = int(0.21*r + 0.72*g + 0.07*b)
    return weighed_avg, weighed_avg, weighed_avg

def hsl_to_rgb(hsl:Tuple[float,float,float]) -> Tuple[int,int,int]:
    """ Converts hsl color to rgb color. """
    h, s, l = hsl

    if s == 0:
        r = g = b = l
    else:
        if l < 0.5: q = l * (1 + s)
        else: q = l + s - l * s
        p = 2 * l - q
        r = hue_to_rgb(p, q, h + 1 / 3)
        g = hue_to_rgb(p, q, h)
        b = hue_to_rgb(p, q, h - 1 / 3)

    return int(round(r * 255)), int(round(g * 255)), int(round(b * 255))

def hue_to_rgb(p:float, q:float, t:float) -> float:
    """ Converts hue to rgb values. Only used by the hsl_to_rgb function. """
    if t < 0: t += 1
    if t > 1: t -= 1
    if t < 1 / 6: return p + (q - p) * 6 * t
    if t < 1 / 2: return q
    if t < 2 / 3: return p + (q - p) * (2 / 3 - t) * 6
    return p

def norm_hsl(h:int, s:int, l:int) -> Tuple[float,float,float]:
    """ Normalize hsl color values. """
    return h/360, s/100, l/100

# Preprocessing ----------------------------------------------------------------

def expand_css_rgb(match) -> str:
    """ Used by the css_to_hex function. """
    return rgb_to_hex((
        int(match.group(1)), int(match.group(2)),
        int(match.group(3)))
    )

def css_to_hex(text:str) -> str:
    """ Returns the given string with css rgba functions and named colors substituted for their corresponding hexadecimal codes. """

    text = re.sub(r"rgba\((\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*,\s*([\d.]+)\)",
                  expand_css_rgb, text)

    text = re.sub(r"rgb\((\d+)\s*,\s*(\d+)\s*,\s*(\d+)\)",
                  expand_css_rgb, text)

    for key in name_to_hex_dict:
        text = re.sub(key + r"\b", name_to_hex_dict[key], text)

    return text

# Post-processing --------------------------------------------------------------

def hex_to_rgba(match) -> str:
    """ Converts 8-digit hexadecimal code to rgba function. """
    hex = match.group(1)

    return f"rgba({int(hex[1:3], 16)}, {int(hex[3:5], 16)}, {int(hex[5:7], 16)}, {int(hex[7:9], 16) / 255.0:.2f})"

def hex_to_css(text:str) -> str:
    """ Convert 8-digit hexadecimal color codes to css rgba color functions. Needed when a css interpreter does not recognize the alpha-channel when reading hexadecimal color codes. """

    return re.sub(r"(#[0-9a-fA-F]{8})", hex_to_rgba, text)

def expand_all_hex(text:str) -> str:
    """Expand all 3-digit hexadecimal codes in the input string to 6 digits."""

    return re.sub(
        r"((?<!&)#[A-Fa-f0-9]{3})\b",
        lambda match: ("#" + "".join([c * 2 for c in match.group(1)[1:]])),
        text
    )

# Color comparision ------------------------------------------------------------

def generate_palette_dict(colors:List[str]) -> Dict[str,Lab]:
    """ Returns a dictionary mapping hexadecimal colors to lab colors. """
    palette_dict = {}

    for color in colors:
        r, g, b = hex_to_rgb(color)
        palette_dict[color] = rgb_to_lab(sRGBColor(r,g,b))

    return palette_dict

def get_input_colors(resource):
    """ Returns an HSL tuple, or a palette of colors, or a color mapping, depending on the input, as well as a string indicating which one, and if smoothing should be applied to pngs/jpgs. """

    # If resource is an hsl color.
    if isinstance(resource, tuple) and len(resource) == 3:
        return resource, False, "color"

    else:
        # If resource is a path to a resource, first unpack.
        if type(resource) is str:
            resource = load_json_file(resource)

        if resource["type"] == "palette":
            return generate_palette_dict(resource["colors"]), resource["smooth"], "palette"

        elif resource["type"] == "mapping":
            return resource["map"], resource["smooth"], "mapping"

def get_file_colors(text:str) -> Set[str]:
    """ Return a set of all unique colors within a given string representing an svg-file. """

    colors = set()
    matches = re.findall(r"#[A-Fa-f0-9]{6}", text)

    for match in matches:
        colors.add(match)

    return colors

def closest_match(color:str, palette:Dict[str,Lab]) -> str:
    """ Compare the similarity of colors in the CIELAB colorspace. Return the closest match, i.e. the palette entry with the smallest euclidian distance to the given color. """

    closest_color = None
    min_distance = float('inf')

    for entry in palette:

        # Prior dictionary lookup and update.
        lab_color = hex_to_lab_dict.get(color)

        if lab_color is None:
            r, g, b = hex_to_rgb(color)
            lab_color = rgb_to_lab(sRGBColor(r,g,b))
            hex_to_lab_dict[color] = lab_color

        distance = get_delta_e_lab(lab_color, palette[entry])

        if distance < min_distance:
            min_distance = distance
            closest_color = entry

    return closest_color

# Pack management --------------------------------------------------------------

def get_paths(folder: str, exts: List[str]) -> List[str]:
    """ Return paths of every file with the given extensions within a folder and its subfolders, excluding symbolic links. """

    paths = []

    for item in os.listdir(folder):
        item_path = os.path.join(folder, item)

        if os.path.islink(item_path): # Link.
            continue

        if os.path.isfile(item_path): # File.
            for ext in exts:
                if item.lower().endswith(ext):
                    paths.append(item_path)

        elif os.path.isdir(item_path): # Folder.
            subfolder_paths = get_paths(item_path, exts)
            paths.extend(subfolder_paths)

    return paths

def copy_file_structure(src_path:str, dest_path:str) -> None:
    """ Copies a directory tree, but changes symbolic links to point to files within the destination folder instead of the source. Assumes that no link points to files outside the source folder. """

    shutil.rmtree(dest_path, ignore_errors=True)
    shutil.copytree(src_path, dest_path, symlinks=True)

    for root, _, files in os.walk(dest_path):
        for file in files:
            file_path = os.path.join(root, file)

            if os.path.islink(file_path):
                link_target = os.readlink(file_path)

                if not os.path.isabs(link_target):
                    continue

                # Make link relative and update.
                link_base = os.path.dirname(file_path)
                relative_target = os.path.relpath(link_target, link_base)
                os.remove(file_path)

                # Replace root source folder with root destination folder.
                relative_target = relative_target.replace(src_path, dest_path, 1)
                os.symlink(relative_target, file_path)

def rename_pack(src_path, dest_path:str, name:str) -> None:
    """ If an index.theme file exists within the given folder, apply appropiate naming. """

    index_path = os.path.join(dest_path, "index.theme")

    print(index_path)
    if os.path.exists(index_path):
        with open(index_path, 'r') as file:
            text = file.read()

        text = re.sub(r"(Name=).*", "\\1" + name, text, count=1)
        text = re.sub(r"(GtkTheme=).*", "\\1" + name, text, count=1)
        text = re.sub(r"(MetacityTheme=).*", "\\1" + name, text, count=1)
        text = re.sub(r"(IconTheme=).*", "\\1" + name, text, count=1)

        text = re.sub(r"(Comment=).*", "\\1" + "A variant of " + os.path.basename(src_path) + " created by nicklasvraa/color-manager", text, count=1)

        with open(index_path, 'w') as file:
            file.write(text)

def copy_pack(src_path:str, dest_path:str, name:str) -> str:
    """ Copy pack and return the resulting copy's directory path. """

    src_path = expand_path(src_path)
    dest_path = os.path.join(expand_path(dest_path), name)

    copy_file_structure(src_path, dest_path)
    rename_pack(src_path, dest_path, name)

    return dest_path

# Vector-based recoloring ------------------------------------------------------

def apply_monotones_to_vec(text:str, colors:Set[str], hsl:Tuple[float,float,float]) -> str:
    """ Replace every instance of color within the given list with their monochrome equivalent in the given string representing an svg-file, determined by the given hue, saturation and lightness offset. """

    h, s, _ = hsl

    if s == 0:
        for color in colors:
            graytone = hex_to_gray(color)
            text = re.sub(color, graytone, text)
    else:
        for color in colors:
            _, __, old_l = hex_to_hsl(color)
            new_color = rgb_to_hex(hsl_to_rgb((h, s, old_l)))
            text = re.sub(color, new_color, text)

    return text

def apply_palette_to_vec(text:str, colors:Set[str], new_colors:Dict[str,Lab]) -> str:
    """ Replace hexadecimal color codes in a given svg/xml/css string with their closest matches within the given color palette. """

    for color in colors:
        new_color = closest_match(color, new_colors)
        h, s, _ = hex_to_hsl(new_color)
        _, __, old_l = hex_to_hsl(color)
        new_color = rgb_to_hex(hsl_to_rgb((h, s, old_l)))
        text = re.sub(color, new_color, text)

    return text

# Pixel-based recoloring -------------------------------------------------------

def apply_monotones_to_img(img:Image, hsl:Tuple[float,float,float]) -> Image:
    """ Replace every instance of color within the given list with their monochrome equivalent in the given image, determined by the given hue, saturation and lightness offset. """

    mode = img.mode
    h, s, _ = hsl

    if s == 0:
        if mode == "RGBA": img = img.convert("LA")
        else: img = img.convert("L")
    else:
        width, height = img.size

        for x in range(width):
            for y in range(height):
                if mode == "RGBA":
                    r, g, b, a = img.getpixel((x, y))
                else:
                    r, g, b = img.getpixel((x, y))

                _, __, old_l = rgb_to_hsl((r, g, b))
                new_color = hsl_to_rgb((h, s, old_l))

                if mode == "RGBA":
                    img.putpixel((x,y), new_color + (a,))
                else:
                    img.putpixel((x,y), new_color)

    return img

def apply_palette_to_img(img:Image, new_colors:Dict[str,Lab], args) -> Image:
    """ Replace colors in a given image with the closest match within a given color palette. """

    if args.smooth: img = img.convert("P", palette=Image.ADAPTIVE, colors=256)
    else: img = img.convert("P")

    palette = img.getpalette()

    rgb_palette = [(palette[i], palette[i+1], palette[i+2]) for i in range(0, len(palette), 3)]

    hex_palette = ["#%02x%02x%02x" % rgb for rgb in rgb_palette]

    new_palette = []
    for color in hex_palette:
        new_color = closest_match(color, new_colors)
        h, s, _ = hex_to_hsl(new_color)
        _, __, old_l = hex_to_hsl(color)
        new_color = hsl_to_rgb((h, s, old_l))
        new_palette.extend(new_color)

    img.putpalette(new_palette)
    return img

# User interface functions -----------------------------------------------------

def recolor_modified(op:str, args) -> None:
    """ Recursively copies and converts a source folder into a destination, given either an hsl color, a palette, or a color mapping. """

    check_path(args.src)

    if op == "palette":
        args.palette = generate_palette_dict(args.palette)

    # Recolor vector graphics.
    paths = get_paths(args.src, [".svg", ".xml"])
    for path in tqdm(paths, desc="svg", disable=is_empty(paths)):
        with open(path, 'r') as file: x = file.read()

        x = css_to_hex(x)
        x = expand_all_hex(x)
        colors = get_file_colors(x)

        if op == "monochrome":
            x = apply_monotones_to_vec(x, colors, random.choice(args.monochrome))
        elif op == "palette":
            x = apply_palette_to_vec(x, colors, args.palette)

        with open(path, 'w') as file: file.write(x)

    # Recolor stylesheets.
    paths = get_paths(args.src, [".css", "rc"])
    for path in tqdm(paths, desc="css", disable=is_empty(paths)):
        with open(path, 'r') as file: x = file.read()

        x = css_to_hex(x)
        x = expand_all_hex(x)
        colors = get_file_colors(x)

        if op == "monochrome":
            x = apply_monotones_to_vec(x, colors, random.choice(args.monochrome))
        elif op == "palette":
            x = apply_palette_to_vec(x, colors, args.palette)

        x = hex_to_css(x)
        with open(path, 'w') as file: file.write(x)

    # Recolor pngs.
    paths = get_paths(args.src, [".png"])
    for path in tqdm(paths, desc="png", disable=is_empty(paths)):
        x = Image.open(path)
        x = x.convert("RGBA")
        a = x.split()[3] # Save original alpha channel.

        if op == "monochrome":
            x = apply_monotones_to_img(x, random.choice(args.monochrome))
        elif op == "palette":
            x = apply_palette_to_img(x, args.palette, args)

        x = x.convert("RGBA")
        r,g,b,_ = x.split()
        x = Image.merge("RGBA",(r,g,b,a)) # Restore original alpha channel.
        x.save(path)

    # Recolor jpgs.
    paths = get_paths(args.src, [".jpg", ".jpeg"])
    for path in tqdm(paths, desc="jpg", disable=is_empty(paths)):
        x = Image.open(path)
        x = x.convert("RGB")

        if op == "monochrome":
            x = apply_monotones_to_img(x, random.choice(args.monochrome))
        elif op == "palette":
            x = apply_palette_to_img(x, args.palette, args)

        x = x.convert("RGB")
        x.save(path)

def list_of_strings(arg):
    return arg.split(',')

def main():
    parser = argparse.ArgumentParser(description='Recolor an image.')

    parser.add_argument('--src', type=str)
    parser.add_argument('--monochrome', type=list_of_strings)
    parser.add_argument('--palette', type=list_of_strings)
    parser.add_argument('--smooth', type=bool, default=True)

    args = parser.parse_args()

    if args.monochrome != None:
        for idx in range(len(args.monochrome)):
            args.monochrome[idx] = hex_to_hsl(args.monochrome[idx])
        recolor_modified("monochrome", args)
    elif args.palette != None:
        recolor_modified("palette", args)

if __name__ == '__main__':
    main()
