# Desc: A program for recoloring icon packs, themes and wallpapers. For NovaOS.
# Auth: Nicklas Vraa

from typing import List, Set, Tuple, Dict, Optional
from tqdm import tqdm
from colormath.color_objects import sRGBColor, LabColor
from colormath.color_conversions import convert_color
from colormath.color_diff import delta_e_cie2000
from PIL import Image, ImageDraw
import os, re, shutil, json, subprocess, argparse, random

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

def expand_css_rgba(match) -> str:
    """ Used by the css_to_hex function. """
    return rgba_to_hex((
        int(match.group(1)), int(match.group(2)),
        int(match.group(3)), float(match.group(4))
    ))

def css_to_hex(text:str) -> str:
    """ Returns the given string with css rgba functions and named colors substituted for their corresponding hexadecimal codes. """

    text = re.sub(r"rgba\((\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*,\s*([\d.]+)\)",
                  expand_css_rgba, text)

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

def generate_palette_dict(colors:List[str]) -> Dict[str,LabColor]:
    """ Returns a dictionary mapping hexadecimal colors to lab colors. """
    palette_dict = {}

    for color in colors:
        r, g, b = hex_to_rgb(color)
        palette_dict[color] = convert_color(sRGBColor(r,g,b), LabColor)

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

def closest_match(color:str, palette:Dict[str,LabColor]) -> str:
    """ Compare the similarity of colors in the CIELAB colorspace. Return the closest match, i.e. the palette entry with the smallest euclidian distance to the given color. """

    closest_color = None
    min_distance = float('inf')

    for entry in palette:

        # Prior dictionary lookup and update.
        lab_color = hex_to_lab_dict.get(color)

        if lab_color is None:
            r, g, b = hex_to_rgb(color)
            lab_color = convert_color(sRGBColor(r,g,b), LabColor)
            hex_to_lab_dict[color] = lab_color

        distance = delta_e_cie2000(lab_color, palette[entry])

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

    h, s, l_offset = hsl

    if s == 0:
        for color in colors:
            graytone = hex_to_gray(color)
            text = re.sub(color, graytone, text)
    else:
        l_offset = (l_offset - 0.5) * 2 # Remapping.

        for color in colors:
            graytone = hex_to_gray(color)
            r, g, b = hex_to_rgb(graytone)
            l = (0.21*r + 0.72*g + 0.07*b)/255
            l = max(0, min(l+l_offset, 1))
            monochrome = rgb_to_hex(hsl_to_rgb((h, s, l)))
            text = re.sub(color, monochrome, text)

    return text

def apply_palette_to_vec(text:str, colors:Set[str], new_colors:Dict[str,LabColor]) -> str:
    """ Replace hexadecimal color codes in a given svg/xml/css string with their closest matches within the given color palette. """

    for color in colors:
        new_color = closest_match(color, new_colors)
        text = re.sub(color, new_color, text)

    return text

def apply_mapping_to_vec(text:str, colors:Set[str], map:Dict[str,str]) -> str:
    """ Replace hexadecimal color codes in a given svg/xml/css string according to a given color mapping. """

    for color in colors:
        if color in map:
            text = re.sub(color, map[color], text)

    return text

# Pixel-based recoloring -------------------------------------------------------

def apply_monotones_to_img(img:Image, hsl:Tuple[float,float,float]) -> Image:
    """ Replace every instance of color within the given list with their monochrome equivalent in the given image, determined by the given hue, saturation and lightness offset. """

    mode = img.mode
    h, s, l_offset = hsl

    if s == 0:
        if mode == "RGBA": img = img.convert("LA")
        else: img = img.convert("L")
    else:
        width, height = img.size
        l_offset = (l_offset - 0.5) * 2 # Remapping.

        for x in range(width):
            for y in range(height):
                if mode == "RGBA":
                    r, g, b, a = img.getpixel((x, y))
                else:
                    r, g, b = img.getpixel((x, y))

                l = (0.21*r + 0.72*g + 0.07*b)/255
                l = max(0, min(l+l_offset, 1))
                new_color = hsl_to_rgb((h, s, l))

                if mode == "RGBA":
                    img.putpixel((x,y), new_color + (a,))
                else:
                    img.putpixel((x,y), new_color)

    return img

def apply_palette_to_img(img:Image, new_colors:Dict[str,LabColor], smooth:bool) -> Image:
    """ Replace colors in a given image with the closest match within a given color palette. """

    if smooth: img = img.convert("P", palette=Image.ADAPTIVE, colors=256)
    else: img = img.convert("P")

    palette = img.getpalette()

    rgb_palette = [(palette[i], palette[i+1], palette[i+2]) for i in range(0, len(palette), 3)]

    hex_palette = ["#%02x%02x%02x" % rgb for rgb in rgb_palette]

    new_palette = []
    for color in hex_palette:
        new_color = hex_to_rgb(closest_match(color, new_colors))
        new_palette.extend(new_color)

    img.putpalette(new_palette)
    return img

def apply_mapping_to_img(img:Image, map:Dict[str,str], smooth:bool) -> Image:
    """ Replace colors in a given image according to a given mapping. """

    #raise Exception("Function not yet implemented.")

    if smooth: img = img.convert("P", palette=Image.ADAPTIVE, colors=256)
    else: img = img.convert("P")

    palette = img.getpalette()

    rgb_palette = [(palette[i], palette[i+1], palette[i+2]) for i in range(0, len(palette), 3)]

    hex_palette = ["#%02x%02x%02x" % rgb for rgb in rgb_palette]

    new_palette = []
    for color in hex_palette:
        if color in map:
            new_palette.extend(hex_to_rgb(map[color]))
        else:
            new_palette.extend(hex_to_rgb(color))

    img.putpalette(new_palette)
    return img

# User interface functions -----------------------------------------------------
def recolor(src_path:str, dest_path:str, name:str, replacement) -> None:
    """ Recursively copies and converts a source folder into a destination, given either an hsl color, a palette, or a color mapping. """

    check_path(src_path)
    check_path(dest_path)

    new_colors, smooth, op = get_input_colors(replacement)
    dest_path = copy_pack(src_path, dest_path, name)

    # Recolor vector graphics.
    paths = get_paths(dest_path, [".svg", ".xml"])
    for path in tqdm(paths, desc="svg", disable=is_empty(paths)):
        with open(path, 'r') as file: x = file.read()

        x = expand_all_hex(x)
        colors = get_file_colors(x)

        if op == "color":
            x = apply_monotones_to_vec(x, colors, new_colors)
        elif op == "palette":
            x = apply_palette_to_vec(x, colors, new_colors)
        elif op == "mapping":
            x = apply_mapping_to_vec(x, colors, new_colors)

        with open(path, 'w') as file: file.write(x)

    # Recolor stylesheets.
    paths = get_paths(dest_path, [".css", "rc"])
    for path in tqdm(paths, desc="css", disable=is_empty(paths)):
        with open(path, 'r') as file: x = file.read()

        x = css_to_hex(x)
        x = expand_all_hex(x)
        colors = get_file_colors(x)

        if op == "color":
            x = apply_monotones_to_vec(x, colors, new_colors)
        elif op == "palette":
            x = apply_palette_to_vec(x, colors, new_colors)
        elif op == "mapping":
            x = apply_mapping_to_vec(x, colors, new_colors)

        x = hex_to_css(x)
        with open(path, 'w') as file: file.write(x)

    # Recolor pngs.
    paths = get_paths(dest_path, [".png"])
    for path in tqdm(paths, desc="png", disable=is_empty(paths)):
        x = Image.open(path)
        x = x.convert("RGBA")
        a = x.split()[3] # Save original alpha channel.

        if op == "color":
            x = apply_monotones_to_img(x, new_colors)
        elif op == "palette":
            x = apply_palette_to_img(x, new_colors, smooth)
        elif op == "mapping":
            x = apply_mapping_to_img(x, new_colors, smooth)

        x = x.convert("RGBA")
        r,g,b,_ = x.split()
        x = Image.merge("RGBA",(r,g,b,a)) # Restore original alpha channel.
        x.save(path)

    # Recolor jpgs.
    paths = get_paths(dest_path, [".jpg", ".jpeg"])
    for path in tqdm(paths, desc="jpg", disable=is_empty(paths)):
        x = Image.open(path)
        x = x.convert("RGB")

        if op == "color":
            x = apply_monotones_to_img(x, new_colors)
        elif op == "palette":
            x = apply_palette_to_img(x, new_colors, smooth)
        elif op == "mapping":
            x = apply_mapping_to_img(x, new_colors, smooth)

        x = x.convert("RGB")
        x.save(path)

def recolor_modified(src_path:str, replacement, op:str, smooth:bool) -> None:
    """ Recursively copies and converts a source folder into a destination, given either an hsl color, a palette, or a color mapping. """

    check_path(src_path)

    if op == "color" or op == "color-from-palette":
        new_colors = replacement
    else:
        new_colors = generate_palette_dict(replacement)

    # Recolor vector graphics.
    paths = get_paths(src_path, [".svg", ".xml"])
    for path in tqdm(paths, desc="svg", disable=is_empty(paths)):
        with open(path, 'r') as file: x = file.read()

        x = expand_all_hex(x)
        colors = get_file_colors(x)

        if op == "color":
            x = apply_monotones_to_vec(x, colors, new_colors)
        elif op == "color-from-palette":
            x = apply_monotones_to_vec(x, colors, random.choice(new_colors))
        elif op == "palette":
            x = apply_palette_to_vec(x, colors, new_colors)

        with open(path, 'w') as file: file.write(x)

    # Recolor stylesheets.
    paths = get_paths(src_path, [".css", "rc"])
    for path in tqdm(paths, desc="css", disable=is_empty(paths)):
        with open(path, 'r') as file: x = file.read()

        x = css_to_hex(x)
        x = expand_all_hex(x)
        colors = get_file_colors(x)

        if op == "color":
            x = apply_monotones_to_vec(x, colors, new_colors)
        elif op == "color-from-palette":
            x = apply_monotones_to_vec(x, colors, random.choice(new_colors))
        elif op == "palette":
            x = apply_palette_to_vec(x, colors, new_colors)

        x = hex_to_css(x)
        with open(path, 'w') as file: file.write(x)

    # Recolor pngs.
    paths = get_paths(src_path, [".png"])
    for path in tqdm(paths, desc="png", disable=is_empty(paths)):
        x = Image.open(path)
        x = x.convert("RGBA")
        a = x.split()[3] # Save original alpha channel.

        if op == "color":
            x = apply_monotones_to_img(x, new_colors)
        elif op == "color-from-palette":
            x = apply_monotones_to_vec(x, colors, random.choice(new_colors))
        elif op == "palette":
            x = apply_palette_to_img(x, new_colors, smooth)

        x = x.convert("RGBA")
        r,g,b,_ = x.split()
        x = Image.merge("RGBA",(r,g,b,a)) # Restore original alpha channel.
        x.save(path)

    # Recolor jpgs.
    paths = get_paths(src_path, [".jpg", ".jpeg"])
    for path in tqdm(paths, desc="jpg", disable=is_empty(paths)):
        x = Image.open(path)
        x = x.convert("RGB")

        if op == "color":
            x = apply_monotones_to_img(x, new_colors)
        elif op == "color-from-palette":
            x = apply_monotones_to_vec(x, colors, random.choice(new_colors))
        elif op == "palette":
            x = apply_palette_to_img(x, new_colors, smooth)

        x = x.convert("RGB")
        x.save(path)

def extract_colors(src_path:str, num_colors:int=8, save_path:str=None, pixels:int=50, cols:int=10) -> List[str]:
    """ Returns and optionally saves the color palette of the given image, as its own image. Optionally specify the number of unique colors you want to be found. """

    check_path(src_path)
    _, ext = os.path.splitext(src_path)

    if ext == ".svg":
        with open(src_path, 'r') as file:
            svg = file.read()

        colors = list(get_file_colors(svg))
        num_colors = len(colors)

    else:
        img = Image.open(src_path)

        colors = img.convert('P', palette=Image.ADAPTIVE, colors=num_colors)
        colors = colors.getpalette()[0:num_colors*3]

        colors = ['#{:02X}{:02X}{:02X}'.format(colors[i], colors[i+1], colors[i+2]) for i in range(0, len(colors), 3)]

    if save_path != None:
        check_path(save_path)

        if num_colors < cols: cols = num_colors

        rows = -(-len(colors) // cols)
        width = cols * pixels; height = rows * pixels

        img = Image.new("RGBA", (width, height))
        draw = ImageDraw.Draw(img)

        for i, hex_color in enumerate(colors):
            row = i // cols; col = i % cols
            x0 = col * pixels; y0 = row * pixels
            x1 = x0 + pixels; y1 = y0 + pixels
            draw.rectangle([x0, y0, x1, y1], fill=hex_color)

        img.save(save_path, format="png")

    return colors

def clean_svg(src_path:str, dest_path:str=None) -> str:
    """ Removes needless metadata from svgs and optionally saves as copy, if output path is specified. """

    check_path(src_path)
    with open(src_path, 'r') as f:
        svg = f.read()

    patterns = [
        r".*xmlns:.*\n",
        r"\s*<metadata[\s\S]*?<\/metadata.*",
        r"\s*<sodipodi[\s\S]*?<\/sodipodi.*",
    ]

    for pattern in patterns:
        svg = re.sub(pattern, '', svg)

    if dest_path is None: dest_path = src_path
    else: check_path(dest_path)

    with open(dest_path, 'w') as f:
        f.write(svg)

def add_backdrop(src_path:str, dest_path:str, name:str, color:str="#000000", padding=0, rounding=0):
    """ Add a customizable backdrop to all svg-based icons. Optionally specify the backdrop color, the padding to the edge of the graphic, and the corner rounding factor. """

    check_path(src_path)
    check_path(dest_path)
    dest_path = copy_pack(src_path, dest_path, name)
    svg_paths = get_paths(dest_path, [".svg"])

    for path in tqdm(svg_paths, desc="Changing svgs  ", unit="file"):
        with open(path, 'r') as file:
            svg = file.read()

        width = int(re.search(r'<svg.*width=\"(\d*)\"', svg).group(1))
        height = int(re.search(r'<svg.*height=\"(\d*)\"', svg).group(1))
        pos = re.search(r'<svg.*>\n', svg).end()

        backdrop = '<rect fill="' + color + '" x="' + str(padding) + '" y="' + str(padding) + '" width="' + str(width-2*padding) + '" height="' + str(height-2*padding) + '" rx="' + str(rounding * (width / 2)) + '" ry="' + str(rounding * (height / 2)) + '"/>'

        credit = "\n<!-- Inserted by Color Manager -->\n"
        svg = svg[:pos] + credit + backdrop + credit + svg[pos:]

        with open(path, 'w') as file:
            file.write(svg)

# Global constants -------------------------------------------------------------

# A dynamic dictionary to avoid multiple color conversions.
hex_to_lab_dict = {
    "#ffffff": LabColor(9341.568974319263, -0.037058350415009045, -0.6906417562959177), # White.
    "#000000": LabColor(0,0,0) # Black.
}

name_to_hex_dict={}
name_to_hex_dict["aliceblue"]="#f0f8ff"
name_to_hex_dict["antiquewhite"]="#faebd7"
name_to_hex_dict["aqua"]="#00ffff"
name_to_hex_dict["aquamarine"]="#7fffd4"
name_to_hex_dict["azure"]="#f0ffff"
name_to_hex_dict["beige"]="#f5f5dc"
name_to_hex_dict["bisque"]="#ffe4c4"
name_to_hex_dict["black"]="#000000"
name_to_hex_dict["blanchedalmond"]="#ffebcd"
name_to_hex_dict["blue"]="#0000ff"
name_to_hex_dict["blueviolet"]="#8a2be2"
name_to_hex_dict["brown"]="#a52a2a"
name_to_hex_dict["burlywood"]="#deb887"
name_to_hex_dict["cadetblue"]="#5f9ea0"
name_to_hex_dict["chartreuse"]="#7fff00"
name_to_hex_dict["chocolate"]="#d2691e"
name_to_hex_dict["coral"]="#ff7f50"
name_to_hex_dict["cornflowerblue"]="#6495ed"
name_to_hex_dict["cornsilk"]="#fff8dc"
name_to_hex_dict["crimson"]="#dc143c"
name_to_hex_dict["cyan"]="#00ffff"
name_to_hex_dict["darkblue"]="#00008b"
name_to_hex_dict["darkcyan"]="#008b8b"
name_to_hex_dict["darkgoldenrod"]="#b8860b"
name_to_hex_dict["darkgray"]="#a9a9a9"
name_to_hex_dict["darkgreen"]="#006400"
name_to_hex_dict["darkgrey"]="#a9a9a9"
name_to_hex_dict["darkkhaki"]="#bdb76b"
name_to_hex_dict["darkmagenta"]="#8b008b"
name_to_hex_dict["darkolivegreen"]="#556b2f"
name_to_hex_dict["darkorange"]="#ff8c00"
name_to_hex_dict["darkorchid"]="#9932cc"
name_to_hex_dict["darkred"]="#8b0000"
name_to_hex_dict["darksalmon"]="#e9967a"
name_to_hex_dict["darkseagreen"]="#8fbc8f"
name_to_hex_dict["darkslateblue"]="#483d8b"
name_to_hex_dict["darkslategray"]="#2f4f4f"
name_to_hex_dict["darkslategrey"]="#2f4f4f"
name_to_hex_dict["darkturquoise"]="#00ced1"
name_to_hex_dict["darkviolet"]="#9400d3"
name_to_hex_dict["deeppink"]="#ff1493"
name_to_hex_dict["deepskyblue"]="#00bfff"
name_to_hex_dict["dimgray"]="#696969"
name_to_hex_dict["dimgrey"]="#696969"
name_to_hex_dict["dodgerblue"]="#1e90ff"
name_to_hex_dict["firebrick"]="#b22222"
name_to_hex_dict["floralwhite"]="#fffaf0"
name_to_hex_dict["forestgreen"]="#228b22"
name_to_hex_dict["fuchsia"]="#ff00ff"
name_to_hex_dict["gainsboro"]="#dcdcdc"
name_to_hex_dict["ghostwhite"]="#f8f8ff"
name_to_hex_dict["goldenrod"]="#daa520"
name_to_hex_dict["gold"]="#ffd700"
name_to_hex_dict["gray"]="#808080"
name_to_hex_dict["green"]="#008000"
name_to_hex_dict["greenyellow"]="#adff2f"
name_to_hex_dict["grey"]="#808080"
name_to_hex_dict["honeydew"]="#f0fff0"
name_to_hex_dict["hotpink"]="#ff69b4"
name_to_hex_dict["indianred"]="#cd5c5c"
name_to_hex_dict["indigo"]="#4b0082"
name_to_hex_dict["ivory"]="#fffff0"
name_to_hex_dict["khaki"]="#f0e68c"
name_to_hex_dict["lavenderblush"]="#fff0f5"
name_to_hex_dict["lavender"]="#e6e6fa"
name_to_hex_dict["lawngreen"]="#7cfc00"
name_to_hex_dict["lemonchiffon"]="#fffacd"
name_to_hex_dict["lightblue"]="#add8e6"
name_to_hex_dict["lightcoral"]="#f08080"
name_to_hex_dict["lightcyan"]="#e0ffff"
name_to_hex_dict["lightgoldenrodyellow"]="#fafad2"
name_to_hex_dict["lightgray"]="#d3d3d3"
name_to_hex_dict["lightgreen"]="#90ee90"
name_to_hex_dict["lightgrey"]="#d3d3d3"
name_to_hex_dict["lightpink"]="#ffb6c1"
name_to_hex_dict["lightsalmon"]="#ffa07a"
name_to_hex_dict["lightseagreen"]="#20b2aa"
name_to_hex_dict["lightskyblue"]="#87cefa"
name_to_hex_dict["lightslategray"]="#778899"
name_to_hex_dict["lightslategrey"]="#778899"
name_to_hex_dict["lightsteelblue"]="#b0c4de"
name_to_hex_dict["lightyellow"]="#ffffe0"
name_to_hex_dict["lime"]="#00ff00"
name_to_hex_dict["limegreen"]="#32cd32"
name_to_hex_dict["linen"]="#faf0e6"
name_to_hex_dict["magenta"]="#ff00ff"
name_to_hex_dict["maroon"]="#800000"
name_to_hex_dict["mediumaquamarine"]="#66cdaa"
name_to_hex_dict["mediumblue"]="#0000cd"
name_to_hex_dict["mediumorchid"]="#ba55d3"
name_to_hex_dict["mediumpurple"]="#9370db"
name_to_hex_dict["mediumseagreen"]="#3cb371"
name_to_hex_dict["mediumslateblue"]="#7b68ee"
name_to_hex_dict["mediumspringgreen"]="#00fa9a"
name_to_hex_dict["mediumturquoise"]="#48d1cc"
name_to_hex_dict["mediumvioletred"]="#c71585"
name_to_hex_dict["midnightblue"]="#191970"
name_to_hex_dict["mintcream"]="#f5fffa"
name_to_hex_dict["mistyrose"]="#ffe4e1"
name_to_hex_dict["moccasin"]="#ffe4b5"
name_to_hex_dict["navajowhite"]="#ffdead"
name_to_hex_dict["navy"]="#000080"
name_to_hex_dict["oldlace"]="#fdf5e6"
name_to_hex_dict["olive"]="#808000"
name_to_hex_dict["olivedrab"]="#6b8e23"
name_to_hex_dict["orange"]="#ffa500"
name_to_hex_dict["orangered"]="#ff4500"
name_to_hex_dict["orchid"]="#da70d6"
name_to_hex_dict["palegoldenrod"]="#eee8aa"
name_to_hex_dict["palegreen"]="#98fb98"
name_to_hex_dict["paleturquoise"]="#afeeee"
name_to_hex_dict["palevioletred"]="#db7093"
name_to_hex_dict["papayawhip"]="#ffefd5"
name_to_hex_dict["peachpuff"]="#ffdab9"
name_to_hex_dict["peru"]="#cd853f"
name_to_hex_dict["pink"]="#ffc0cb"
name_to_hex_dict["plum"]="#dda0dd"
name_to_hex_dict["powderblue"]="#b0e0e6"
name_to_hex_dict["purple"]="#800080"
name_to_hex_dict["rebeccapurple"]="#663399"
name_to_hex_dict["red"]="#ff0000"
name_to_hex_dict["rosybrown"]="#bc8f8f"
name_to_hex_dict["royalblue"]="#4169e1"
name_to_hex_dict["saddlebrown"]="#8b4513"
name_to_hex_dict["salmon"]="#fa8072"
name_to_hex_dict["sandybrown"]="#f4a460"
name_to_hex_dict["seagreen"]="#2e8b57"
name_to_hex_dict["seashell"]="#fff5ee"
name_to_hex_dict["sienna"]="#a0522d"
name_to_hex_dict["silver"]="#c0c0c0"
name_to_hex_dict["skyblue"]="#87ceeb"
name_to_hex_dict["slateblue"]="#6a5acd"
name_to_hex_dict["slategray"]="#708090"
name_to_hex_dict["slategrey"]="#708090"
name_to_hex_dict["snow"]="#fffafa"
name_to_hex_dict["springgreen"]="#00ff7f"
name_to_hex_dict["steelblue"]="#4682b4"
name_to_hex_dict["tan"]="#d2b48c"
name_to_hex_dict["teal"]="#008080"
name_to_hex_dict["thistle"]="#d8bfd8"
name_to_hex_dict["tomato"]="#ff6347"
name_to_hex_dict["turquoise"]="#40e0d0"
name_to_hex_dict["violet"]="#ee82ee"
name_to_hex_dict["wheat"]="#f5deb3"
name_to_hex_dict["white"]="#ffffff"
name_to_hex_dict["whitesmoke"]="#f5f5f5"
name_to_hex_dict["yellow"]="#ffff00"
name_to_hex_dict["yellowgreen"]="#9acd32"

def list_of_strings(arg):
    return arg.split(',')

def main():
    parser = argparse.ArgumentParser(description='Recolor an image.')

    parser.add_argument('--src', type=str)
    parser.add_argument('--color', type=str)
    parser.add_argument('--color-from-palette', type=list_of_strings)
    parser.add_argument('--palette', type=list_of_strings)
    parser.add_argument('--smooth', type=bool, default=True)

    args = parser.parse_args()

    if args.color != None:
        recolor_modified(args.src, hex_to_hsl(args.color), "color", args.smooth)
    if args.color_from_palette != None:
        for idx in range(len(args.color_from_palette)):
            args.color_from_palette[idx] = hex_to_hsl(args.color_from_palette[idx])

        recolor_modified(args.src, args.color_from_palette, "color-from-palette", args.smooth)
    if args.palette != None:
        recolor_modified(args.src, args.palette, "palette", args.smooth)

if __name__ == '__main__':
    main()
