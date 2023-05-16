### Styling Guidelines
The base16 style guide is generally targeted toward text editors. stylix, however aims to support a verity of applications, and as such it requires its own guide to keep colors consistent across applications. 
towards this goal we will define several common types of applications and how to style each of them with the base16 color scheme. keep in mind this is a general theming guide, there will be several applications that does not fit these groups, in which case it is up to the committer to make sure said application fits in stylistically with the rest of the themed applications.

## General Applications
General applications are applications that do not fit into the mold of any of the applications below,
this is the scheme you should default to if you are unsure. examples include zathura and sxiv.
- Default Background: base00 
- Alternate Background: base01
- Selection Color: base03
- Alternate Text Color: base04
- Default Text Color: base05
- Urgent: base07
- Error: base0F

## Window Managers/Desktop Enviornments
Window Managers and Desktop Environments are the system used to manage windows. this does not include applications bundled with Desktop Environment's like file managers, these would be included under the general application category. examples include sway, gnome, kde, and i3.

- Default background: base00
- Alternate Background: base01
- Preselection background: base02
- Unfocused window border: base03
- Alternate Text Color: base04
- Default Text Color: base05
- Color of urgent windows: base07
- Focused window border: base0A
- Error: base0F


## Notifications and Popups
Notifications and popups are any application overlay intended to be displayed over other applications. examples include the mako notification daemon and avizo.

- Default Background Color: base00
- Alternate Background: base01
- Progress/Percentage Bar Color: base02
- Alternate Text Color: base04
- Default Text Color: base05
- low Urgency Background Color: base06
- High Urgency Text Color: base08
- Low Urgency Text Color: base0A
- Border Color: base0D
- High Urgency Background Color: base0F

## Desktop Helpers
Applications that fall under this groups are applications that complement the window management facilities of whatever window manager the user is using. examples of this include waybar and polybar.

- Default Background Color: base00
- Alternate Background: base01
- Alternate Item Off: base02
- Selected Item In List: base03
- Alternate Text Color: base04
- Default Text Color: base05
- Urgent: base07
- Alternate Item On: base09
- Item Off, Default List Background: base0D
- Item On: base0E
- Error: base0F

## colorable images
for coloring images like wallpapers and icons

- Background Color: base00
- Alternate Background Color: base01
- Alternate Text Color: base04
- Text color: base05
- Red: base08
- orange: base09
- yellow: base0A
- green: base0B
- cyan: base0C
- blue: base0D
- purple: base0E
- brown: base0F



## Text Editors/Viewers
text editors and viewers are any application that can view and/or edit program source code. examples include vim, helix, and bat.
[for these applications see the base16 style guide](https://github.com/chriskempson/base16/blob/main/styling.md)
