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

It is also important to note that this is a growing theming guide and when theming an application and you find the guide to be lacking in any way in
terms of direction, you are encouraged to open an issue regarding what you would like to see added to the style guide.

## Terms

### Alternate

An alternate color should be used when something needs to look separate while not
being drastically different. The smaller or less common element should use the
alternate color.

![Appearance tab in GNOME settings](https://github.com/SomeGuyNamedMy/stylix/assets/28959268/e29f9fec-7b68-45ce-95ef-90d8e787c991)

For example, each section in this settings menu uses the alternate background color
to separate it from the rest of the window, which is using the default background.

### On/Off

This is for toggles or simple status indicators which have an obvious on and off state.

![Toggles in GNOME quick settings](https://github.com/SomeGuyNamedMy/stylix/assets/28959268/710056f6-26f7-47d4-bd2f-1384185fb46a)

In the screenshot above the Wired and Night Light buttons are on, Power Mode is off.

### Lists and selections

A list of items to select between, such as tabs in a web browser. The selection is
the currently active item, or there could be multiple selected depending on the use case.

![Sidebar of Nautilus file manager](https://github.com/SomeGuyNamedMy/stylix/assets/28959268/3b893677-75e1-4190-b3ab-b07d10930b19)

## General colors

- Default background: base00
- Alternate background: base01
- Selection background: base02
- Default text: base05
- Alternate text: base04
- Warning: base0A
- Urgent: base09
- Error: base08

## Window Managers

Window Managers arrange windows and provide decorations like title bars and
borders. Examples include Sway and i3.

This does not include applications bundled with the desktop environment such as
file managers, which would fall into the general category. Desktop helpers such as
taskbars and menus are not technically part of the window manager, although they're
often configured in the same place.

An urgent window is one which is grabbing for attention - Windows shows this by
a flashing orange taskbar icon.

- Unfocused window border: base03
- Focused window border: base0D
- Unfocused window border in group: base03
- Focused window border in group: base0D
- Urgent window border: base08
- Window title text: base05

## Notifications and Popups

Notifications and popups are any application overlay intended to be displayed
over other applications. Examples include the mako notification daemon and
avizo.

- Window border: base0D
- Low urgency background color: base06
- Low urgency text color: base0A
- High urgency background color: base0F
- High urgency text color: base08
- Incomplete part of progress bar: base01
- Complete part of progress bar: base02

## Desktop Helpers

Applications that fall under this group are applications that complement the
window management facilities of whatever window manager the user is using.
Examples of this include waybar and polybar, as well as the similar programs
that are part of KDE and GNOME.

### Light text color widgets

Refer to general colors above.

### Dark text color widgets

These widgets use a different text color than usual to ensure it's still
readable when the background is more vibrant.

- Default text color: base00
- Alternate text color: base01
- Item on background color: base0E
- Item off background color: base0D
- Alternate item on background color: base09
- Alternate item off background color: base02
- List unselected background: base0D
- List selected background: base03

## Images

For creating modified versions of logos, icons, etc; where we would rather the
colors be similar to the original.

Note that the colors provided by the scheme won't necessarily match the names given
below, although most handmade schemes do.

- Background color: base00
- Alternate background color: base01
- Main color: base05
- Alternate main color: base04
- Red: base08
- Orange: base09
- Yellow: base0A
- Green: base0B
- Cyan: base0C
- Blue: base0D
- Purple: base0E
- Brown: base0F

![Recolored systemd logo](https://github.com/SomeGuyNamedMy/stylix/assets/28959268/00ba9b23-c7eb-4cbf-9f3d-aa8de159d6dd)

Example of a modified systemd logo. The square brackets are using the main color,
which is usually be white or black depending on the polarity of the scheme.

## Text Editors/Viewers

Text editors are any application that can view or edit source code.
Examples include vim, helix, and bat.

For these please refer to the official
[base16 style guide](https://github.com/chriskempson/base16/blob/main/styling.md).
