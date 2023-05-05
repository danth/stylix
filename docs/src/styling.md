### Styling Guidelines
The base16 style guide is generally targeted toward text editors. stylix, however aims to support a verity of applications, and as such it requires its own guide to keep colors consistent across applications. 
towards this goal we will define several common types of applications and how to style each of them with the base16 color scheme. keep in mind this is a general theming guide, there will be several applications that does not fit these groups, in which case it is up to the committer to make sure said application fits in stylistically with the rest of the themed applications.

## General Applications
General applications are applications that do not fit into the mold of any of the applications below,
this is the scheme you should default to if you are unsure. examples include zathura and sxiv.

- base00 - Default Background
- base01 - Alternate Background
- base02 - TODO
- base03 - Selection Color
- base04 - Alternate Text Color
- base05 - Default Text Color
- base06 - TODO
- base07 - Urgent
- base08 - TODO
- base09 - TODO
- base0A - TODO
- base0B - TODO
- base0C - TODO
- base0D - TODO
- base0E - TODO
- base0F - Error

## Text Editors/Viewers
text editors and viewers are any application that can view and/or edit program source code. examples include vim, helix, and bat.

- base00 - Default Background
- base01 - Lighter Background (Used for status bars, line number and folding marks)
- base02 - Selection Background
- base03 - Comments, Invisibles, Line Highlighting
- base04 - Dark Foreground (Used for status bars)
- base05 - Default Foreground, Caret, Delimiters, Operators
- base06 - Light Foreground (Not often used)
- base07 - Light Background (Not often used)
- base08 - Variables, XML Tags, Markup Link Text, Markup Lists, Diff Deleted
- base09 - Integers, Boolean, Constants, XML Attributes, Markup Link Url
- base0A - Classes, Markup Bold, Search Text Background
- base0B - Strings, Inherited Class, Markup Code, Diff Inserted
- base0C - Support, Regular Expressions, Escape Characters, Markup Quotes
- base0D - Functions, Methods, Attribute IDs, Headings
- base0E - Keywords, Storage, Selector, Markup Italic, Diff Changed
- base0F - Deprecated, Opening/Closing Embedded Language Tags, e.g. `<?php ?>`

## Window Managers/Desktop Enviornments
Window Managers and Desktop Environments are the system used to manage windows. this does not include applications bundled with Desktop Environment's like file managers, these would be included under the general application category. examples include sway, gnome, kde, and i3.

- base00 - Default background when wallpaper is not set
- base01 - Alternate Background
- base02 - Preselection background
- base03 - Unfocused window border
- base04 - Alternate Text Color
- base05 - Default Text Color
- base06 - TODO
- base07 - Color of urgent windows
- base08 - TODO
- base09 - TODO
- base0A - Focused window border
- base0B - TODO
- base0C - TODO
- base0D - TODO
- base0E - TODO
- base0F - Error


## Notifications and Popups
Notifications and popups are any application overlay intended to be displayed over other applications. examples include the mako notification daemon and avizo.

- base00 - Default Background Color
- base01 - Alternate Background
- base02 - Progress/Percentage Bar Color
- base03 - TODO
- base04 - Alternate Text Color
- base05 - Default Text Color
- base06 - low Urgency Background Color
- base07 - TODO
- base08 - High Urgency Text Color
- base09 - TODO
- base0A - Low Urgency Text Color
- base0B - TODO
- base0C - TODO
- base0D - Border Color
- base0E - TODO
- base0F - High Urgency Background Color

## Desktop Helpers
Applications that fall under this groups are applications that complement the window management facilities of whatever window manager the user is using. examples of this include waybar and polybar.

- base00 - Default Background Color
- base01 - Alternate Background
- base02 - Alternate Item Off (for example internt disconected or media player off)
- base03 - Selected Item In List
- base04 - Alternate Text Color
- base05 - Default Text Color
- base06 - Static Item
- base07 - Urgent
- base08 - Static Item
- base09 - Alternate Item On (for example internt disconected or media player off)
- base0A - Static Item
- base0B - Static Item
- base0C - Static Item
- base0D - Item Off (for example item discharging or sound muted), Default List Background
- base0E - Item On (for example item charging or with sound on)
- base0F - Error

## colorable images
for coloring images like wallpapers and icons

- base00 - TODO
- base01 - TODO
- base02 - TODO
- base03 - TODO
- base04 - TODO
- base05 - TODO
- base06 - TODO
- base07 - TODO
- base08 - Red
- base09 - orange
- base0A - yellow
- base0B - green
- base0C - cyan
- base0D - blue
- base0E - purple
- base0F - brown

## Credits
![Base16 Style Guide]("https://github.com/chriskempson/base16/blob/main/styling.md")
