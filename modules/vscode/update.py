#! /usr/bin/env nix-shell
#! nix-shell -i python3 -p python3Packages.markdown-it-py python3Packages.requests

# This script parses the reference page and updates our template with any
# added or removed settings.
#
# New settings are null by default. VSCode can handle this gracefully, but
# normally a value should be chosen before committing the update.
# 
# This only updates 'colors' and not 'tokenColors' for now.

import json
import os
from markdown_it import MarkdownIt
import requests

documentation = requests.get('https://raw.githubusercontent.com/microsoft/vscode-docs/main/api/references/theme-color.md').text

color_names = [
    token.children[0].content
    for token in MarkdownIt().parse(documentation)
    if token.type == 'inline' and token.children[0].type == 'code_inline'
]

template_path = os.path.join(os.path.dirname(__file__), 'template.mustache')

with open(template_path, 'r') as template_file:
    template = json.load(template_file)

template['colors'] = {
    color_name: template['colors'].get(color_name)
    for color_name in color_names
}

with open(template_path, 'w') as template_file:
    json.dump(template, template_file, indent=4)