{
  lib,
  config,
  ...
}:
let
  cfg = config.stylix.targets.plymouth;

  backgroundColor =
    with config.lib.stylix.colors;
    "${base00-dec-r}, ${base00-dec-g}, ${base00-dec-b}";

  foregroundColor =
    with config.lib.stylix.colors;
    "${base05-dec-r}, ${base05-dec-g}, ${base05-dec-b}";
in
builtins.toFile "stylix-plymouth-theme" ''
  center_x = Window.GetWidth() / 2;
  center_y = Window.GetHeight() / 2;
  baseline_y = Window.GetHeight() * 0.9;
  message_y = Window.GetHeight() * 0.75;

  ### BACKGROUND ###

  Window.SetBackgroundTopColor(${backgroundColor});
  Window.SetBackgroundBottomColor(${backgroundColor});

  ### LOGO ###

  logo.image = Image("logo.png");
  logo.sprite = Sprite(logo.image);
  logo.sprite.SetPosition(
    center_x - (logo.image.GetWidth() / 2),
    center_y - (logo.image.GetHeight() / 2),
    1
  );

  logo.spinner_active = ${if cfg.logoAnimated then "1" else "0"};
  logo.spinner_third = 0;
  logo.spinner_index = 0;
  logo.spinner_max_third = 32;
  logo.spinner_max = logo.spinner_max_third * 3;

  real_index = 0;
  for (third = 0; third < 3; third++) {
    for (index = 0; index < logo.spinner_max_third; index++) {
      subthird = index / logo.spinner_max_third;
      angle = (third + ((Math.Sin(Math.Pi * (subthird - 0.5)) / 2) + 0.5)) / 3;
      logo.spinner_image[real_index] = logo.image.Rotate(2*Math.Pi * angle);
      real_index++;
    }
  }

  fun activate_spinner () {
    logo.spinner_active = 1;
  }

  fun deactivate_spinner () {
    logo.spinner_active = 0;
    logo.sprite.SetImage(logo.image);
  }

  fun refresh_callback () {
    if (logo.spinner_active) {
      logo.spinner_index = (logo.spinner_index + 1) % (logo.spinner_max * 2);
      logo.sprite.SetImage(logo.spinner_image[Math.Int(logo.spinner_index / 2)]);
    }
  }

  Plymouth.SetRefreshFunction(refresh_callback);

  ### PASSWORD ###

  prompt = null;
  bullets = null;
  bullet.image = Image.Text("•", ${foregroundColor});

  fun password_callback (prompt_text, bullet_count) {
    ${lib.optionalString cfg.logoAnimated "deactivate_spinner();"}

    prompt.image = Image.Text("Enter password", ${foregroundColor});
    prompt.sprite = Sprite(prompt.image);
    prompt.sprite.SetPosition(
      center_x - (prompt.image.GetWidth() / 2),
      baseline_y - prompt.image.GetHeight(),
      1
    );

    total_width = bullet_count * bullet.image.GetWidth();
    start_x = center_x - (total_width / 2);

    bullets = null;
    for (i = 0; i < bullet_count; i++) {
        bullets[i].sprite = Sprite(bullet.image);
        bullets[i].sprite.SetPosition(
          start_x + (i * bullet.image.GetWidth()),
          baseline_y + bullet.image.GetHeight(),
          1
        );
    }
  }

  Plymouth.SetDisplayPasswordFunction(password_callback);

  ### QUESTION ###

  question = null;
  answer = null;

  fun question_callback(prompt_text, entry) {
      ${lib.optionalString cfg.logoAnimated "deactivate_spinner();"}

      question = null;
      answer = null;

      question.image = Image.Text(prompt_text, ${foregroundColor});
      question.sprite = Sprite(question.image);
      question.sprite.SetPosition(
          center_x - (question.image.GetWidth() / 2),
          baseline_y - question.image.GetHeight(),
          1
      );

      answer.image = Image.Text(entry, ${foregroundColor});
      answer.sprite = Sprite(answer.image);
      answer.sprite.SetPosition(
          center_x - (answer.image.GetWidth() / 2),
          baseline_y + answer.image.GetHeight(),
          1
      );
  }

  Plymouth.SetDisplayQuestionFunction(question_callback);

  ### MESSAGE ###

  message = null;

  fun message_callback(text) {
      message.image = Image.Text(text, ${foregroundColor});
      message.sprite = Sprite(message.image);
      message.sprite.SetPosition(
          center_x - message.image.GetWidth() / 2,
          message_y,
          1
      );
  }

  Plymouth.SetMessageFunction(message_callback);

  ### NORMAL ###

  fun normal_callback() {
      prompt = null;
      bullets = null;

      question = null;
      answer = null;

      message = null;

      ${lib.optionalString cfg.logoAnimated "activate_spinner();"}
  }

  Plymouth.SetDisplayNormalFunction(normal_callback);

  ### QUIT ###

  fun quit_callback() {
    prompt = null;
    bullets = null;
    ${lib.optionalString cfg.logoAnimated "deactivate_spinner();"}
  }

  Plymouth.SetQuitFunction(quit_callback);
''
