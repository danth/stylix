#! /usr/bin/env nix-shell
#! nix-shell -i python3 -p python3Packages.markdown-it-py python3Packages.requests

# This script parses the reference page and updates our template with any
# added or removed settings, then sorts the template alphabetically.
#
# New settings are null by default. VSCode can handle this gracefully, but
# normally a value should be chosen before committing the update.
#
# This only updates 'colors' and not 'tokenColors' for now.

import os
from markdown_it import MarkdownIt
import requests

documentation = requests.get('https://raw.githubusercontent.com/microsoft/vscode-docs/main/api/references/theme-color.md').text

color_names = [
    token.children[0].content
    for token in MarkdownIt().parse(documentation)
    if token.type == 'inline' and token.children[0].type == 'code_inline'
]

template_path = os.path.join(os.path.dirname(__file__), 'templates/theme.nix')

before_lines = []
color_lines = []
after_lines = []

with open(template_path, 'r') as template_file:
    while line := template_file.readline():
        before_lines.append(line)

        if line == '  colors = {\n':
            break

    while line := template_file.readline():
        if line == '  };\n':
            after_lines.append(line)
            break

        name = line.split('"')[1]

        if name in color_names:
            color_lines.append(line)
            color_names.remove(name)
        else:
            print('-', name)

    for name in color_names:
        print('+', name)
        color_lines.append(f'    "{name}" = null;\n')

    color_lines.sort()

    after_lines.extend(template_file.readlines())

with open(template_path, 'w') as template_file:
    template_file.writelines(before_lines + color_lines + after_lines)
