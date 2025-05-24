{ mkTarget, ... }:
mkTarget {
  name = "halloy";
  humanName = "Halloy";

  configElements =
    { colors }:
    {
      programs.halloy = {
        settings.theme = "stylix";
        themes.stylix = with colors.withHashtag; {
          general = {
            background = base00;
            border = base07;
            horizontal_rule = base02;
            unread_indicator = base0A;
          };
          text = {
            primary = base05;
            secondary = base04;
            tertiary = base0A;
            success = base0B;
            error = base08;
          };
          buffer = {
            action = base0B;
            background = base00;
            background_text_input = base01;
            background_title_bar = base01;
            border = base03;
            border_selected = base07;
            code = base0E;
            highlight = base01;
            nickname = base0C;
            selection = base02;
            timestamp = base05;
            topic = base04;
            url = base0D;
            server_messages = {
              join = base0B;
              part = base08;
              quit = base08;
              default = base0C;
            };
          };
          buttons = {
            primary = {
              background = base00;
              background_hover = base02;
              background_selected = base03;
              background_selected_hover = base04;
            };
            secondary = {
              background = base01;
              background_hover = base02;
              background_selected = base03;
              background_selected_hover = base04;
            };
          };
        };
      };
    };
}
