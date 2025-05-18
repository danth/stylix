{ colors }:
with colors;
{
  Metadata = {
    Name = "Stylix";
    Version = 0.1;
    Author = "sanweiya and ontake (make-42)";
    Description = "Stylix fcitx5 theme based on the mellow-themes by sanweiya";
  };
  InputPanel = {
    NormalColor = base05;
    HighlightCandidateColor = base06;
    EnableBlur = false;
    FullWidthHighlight = true;
    HighlightColor = base04;
    HighlightBackgroundColor = base0E;
  };
  "InputPanel/BlurMargin" = {
    Left = 0;
    Right = 0;
    Top = 0;
    Bottom = 0;
  };
  "InputPanel/Background" = {
    Image = "panel.svg";
    Color = base01;
    BorderColor = base01;
    BorderWidth = 0;
    Gravity = "Top Left";
    OverlayOffsetX = 0;
    OverlayOffsetY = 0;
    HideOverlayIfOversize = false;
  };
  "InputPanel/Background/Margin" = {
    Left = 15;
    Right = 15;
    Top = 15;
    Bottom = 15;
  };
  "InputPanel/Background/OverlayClipMargin" = {
    Left = 0;
    Right = 0;
    Top = 0;
    Bottom = 0;
  };
  "InputPanel/Highlight" = {
    Image = "highlight.svg";
    Color = base01;
    BorderColor = base01 + "00";
    BorderWidth = 0;
    Gravity = "Top Left";
    OverlayOffsetX = 0;
    OverlayOffsetY = 0;
    HideOverlayIfOversize = false;
  };
  "InputPanel/Highlight/Margin" = {
    Left = 15;
    Right = 15;
    Top = 10;
    Bottom = 10;
  };
  "InputPanel/Highlight/OverlayClipMargin" = {
    Left = 0;
    Right = 0;
    Top = 0;
    Bottom = 0;
  };
  "InputPanel/Highlight/HighlightClickMargin" = {
    Left = 0;
    Right = 0;
    Top = 0;
    Bottom = 0;
  };
  "InputPanel/ContentMargin" = {
    Left = 9;
    Right = 9;
    Top = 7;
    Bottom = 7;
  };
  "InputPanel/TextMargin" = {
    Left = 9;
    Right = 9;
    Top = 6;
    Bottom = 7;
  };
  "InputPanel/PrevPage" = {
  };
  "InputPanel/PrevPage/ClickMargin" = {
    Left = 5;
    Right = 5;
    Top = 4;
    Bottom = 4;
  };
  "InputPanel/NextPage" = {
  };
  "InputPanel/NextPage/ClickMargin" = {
    Left = 5;
    Right = 5;
    Top = 4;
    Bottom = 4;
  };
  "InputPanel/ShadowMargin" = {
    Left = 0;
    Right = 0;
    Top = 0;
    Bottom = 0;
  };
  Menu = {
    NormalColor = base05;
    HighlightCandidateColor = base02;
    Spacing = 0;
  };
  "Menu/Background" = {
    Image = "panel.svg";
    Color = base01;
    BorderColor = base01 + "00";
    BorderWidth = 0;
    Gravity = "Top Left";
    OverlayOffsetX = 0;
    OverlayOffsetY = 0;
    HideOverlayIfOversize = false;
  };
  "Menu/Background/Margin" = {
    Left = 11;
    Right = 11;
    Top = 11;
    Bottom = 11;
  };
  "Menu/Background/OverlayClipMargin" = {
    Left = 0;
    Right = 0;
    Top = 0;
    Bottom = 0;
  };
  "Menu/Highlight" = {
    Image = "highlight.svg";
    Color = base05;
    BorderColor = base05 + "00";
    BorderWidth = 0;
    Gravity = "Top Left";
    OverlayOffsetX = 0;
    OverlayOffsetY = 0;
    HideOverlayIfOversize = false;
  };
  "Menu/Highlight/Margin" = {
    Left = 5;
    Right = 5;
    Top = 5;
    Bottom = 5;
  };
  "Menu/Highlight/OverlayClipMargin" = {
    Left = 0;
    Right = 0;
    Top = 0;
    Bottom = 0;
  };
  "Menu/Separator" = {
    Color = base00;
    BorderColor = base00 + "00";
    BorderWidth = 0;
    Gravity = "Top Left";
    OverlayOffsetX = 0;
    OverlayOffsetY = 0;
    HideOverlayIfOversize = false;
  };
  "Menu/Separator/Margin" = {
    Left = 0;
    Right = 0;
    Top = 0;
    Bottom = 0;
  };
  "Menu/Separator/OverlayClipMargin" = {
    Left = 0;
    Right = 0;
    Top = 0;
    Bottom = 0;
  };
  "Menu/CheckBox" = {
    Image = "radio.svg";
    Color = base01;
    BorderColor = base01 + "00";
    BorderWidth = 0;
    Gravity = "Top Left";
    OverlayOffsetX = 0;
    OverlayOffsetY = 0;
    HideOverlayIfOversize = false;
  };
  "Menu/CheckBox/Margin" = {
    Left = 0;
    Right = 0;
    Top = 0;
    Bottom = 0;
  };
  "Menu/CheckBox/OverlayClipMargin" = {
    Left = 0;
    Right = 0;
    Top = 0;
    Bottom = 0;
  };
  "Menu/SubMenu" = {
    Image = "arrow.svg";
    Color = base01;
    BorderColor = base01 + "00";
    BorderWidth = 0;
    Gravity = "Top Left";
    OverlayOffsetX = 0;
    OverlayOffsetY = 0;
    HideOverlayIfOversize = false;
  };
  "Menu/SubMenu/Margin" = {
    Left = 0;
    Right = 0;
    Top = 0;
    Bottom = 0;
  };
  "Menu/SubMenu/OverlayClipMargin" = {
    Left = 0;
    Right = 0;
    Top = 0;
    Bottom = 0;
  };
  "Menu/ContentMargin" = {
    Left = 11;
    Right = 11;
    Top = 11;
    Bottom = 11;
  };
  "Menu/TextMargin" = {
    Left = 6;
    Right = 6;
    Top = 6;
    Bottom = 6;
  };
}
