{ fonts }:
{
  stylix.targets.blender.themeBody = ''
    <ThemeStyle>
      <panel_title>
        <ThemeFontStyle
          points="${toString fonts.sizes.desktop}"
          character_weight="400"
          shadow="1"
          shadow_offset_x="0"
          shadow_offset_y="-1"
          shadow_alpha="0.15"
          shadow_value="1"
        ></ThemeFontStyle>
      </panel_title>
      <widget_label>
        <ThemeFontStyle
          points="${toString fonts.sizes.popups}"
          character_weight="400"
          shadow="3"
          shadow_offset_x="0"
          shadow_offset_y="-1"
          shadow_alpha="0.15"
          shadow_value="1"
        ></ThemeFontStyle>
      </widget_label>
      <widget>
        <ThemeFontStyle
          points="${toString fonts.sizes.popups}"
          character_weight="400"
          shadow="0"
          shadow_offset_x="0"
          shadow_offset_y="0"
          shadow_alpha="0.25"
          shadow_value="0"
        ></ThemeFontStyle>
      </widget>
    </ThemeStyle>
  '';
}
