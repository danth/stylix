# Style guide

The [base16 style guide](https://github.com/chriskempson/base16/blob/main/styling.md)
is generally targeted towards text editors. Stylix aims to support a variety of
other applications, and as such it requires its own guide to keep colours
consistent. Towards this goal we will define several common types of
applications and how to style each of them using the available colours.

Please keep in mind that this is a general guide; there will be several
applications that don't fit into any of the groups below. In this case it is up
to the committer to make sure said application fits in stylistically with the
rest of the themed applications.

### Terms
- Alternate: To use when to items need to contrast despite being the same type of item.
- Item On/Off: This is for on off indicators in applications like a battery charging indicator.
- List: A list of items to select between such as workspaces in a desktop bar.

## General Applications

General applications are applications that do not fit into any of the more
specific categories below. This is the scheme you should default to if you are
unsure. Examples include zathura and sxiv.

- Default Background: base00 
- Alternate Background: base01
- Default Text Color: base05
- Alternate Text Color: base04
- Background of Sidebar Selection Color: base03
- Urgent: base07
- Error: base0F

## Window Managers

Window Managers arrange windows and provide decorations like title bars and
borders. Examples include Sway and i3.

This does not include applications bundled with the desktop environment such as
file managers, which would fall into the general category.

It also does not include desktop helpers such as taskbars and menus. However,
these are often configured together with the window manager, especially in the
case of larger desktop environments such as KDE and GNOME.

- Unfocused window border: base03
- Focused window border: base0A
- Urgent window border: base07
- Window title text: base05
- Focused Window in Group: base6
- Unfocused Window in Group: base0D
- Error: base0F


## Notifications and Popups

Notifications and popups are any application overlay intended to be displayed
over other applications. Examples include the mako notification daemon and
avizo.

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

Applications that fall under this group are applications that complement the
window management facilities of whatever window manager the user is using.
Examples of this include waybar and polybar, as well as the similar programs
that are part of KDE and GNOME.

### Light Text Color Widgets
- Default Background Color: base00
- Alternate Background Color: base01
- Default Text Color: base05
- Alternate Text Color: base04

### dark Text Color Widgets
- Default Text Color: base00
- Alternate Text Color: base01
- Item On: base0E
- Item Off: base0D
- Alternate Item On: base09
- Alternate Item Off: base02
- List Background: base0D
- Selected Item In List Background: base03
- Urgent: base07
- Error: base0F

## Images

For creating modified versions of logos, icons, etc.

- Background Color: base00
- Alternate Background Color: base01
- Text color: base05
- Alternate text color: base04
- Red: base08
- Orange: base09
- Yellow: base0A
- Green: base0B
- Cyan: base0C
- Blue: base0D
- Purple: base0E
- Brown: base0F

## Text Editors/Viewers

Text editors are any application that can view or edit source code.
Examples include vim, helix, and bat.

For these please refer to the official
[base16 style guide](https://github.com/chriskempson/base16/blob/main/styling.md).
