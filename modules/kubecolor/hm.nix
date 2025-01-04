{ config, lib, ... }:
{
  options.stylix.targets.kubecolor.enable =
    config.lib.stylix.mkEnableTarget "kubecolor" true;

  config = lib.mkIf config.stylix.targets.kubecolor.enable {
    programs.kubecolor.settings = {
      preset =
        if config.stylix.polarity == "either" then "" else "${config.stylix.polarity}";
      theme = {
        base = {
          info = "fg=${config.lib.stylix.colors.withHashtag.base05-hex}";
          primary = "fg=${config.lib.stylix.colors.withHashtag.base0E-hex}";
          secondary = "fg=${config.lib.stylix.colors.withHashtag.base0D-hex}";
          success = "fg=${config.lib.stylix.colors.withHashtag.base0B-hex}:bold";
          warning = "fg=${config.lib.stylix.colors.withHashtag.base0A-hex}:bold";
          danger = "fg=${config.lib.stylix.colors.withHashtag.base08-hex}:bold";
          muted = "fg=${config.lib.stylix.colors.withHashtag.base04-hex}";
          key = "fg=${config.lib.stylix.colors.withHashtag.base07-hex}:bold";
        };
        default = "fg=${config.lib.stylix.colors.withHashtag.base05-hex}";
        data = {
          key = "fg=${config.lib.stylix.colors.withHashtag.base07-hex}:bold";
          string = "fg=${config.lib.stylix.colors.withHashtag.base05-hex}";
          true = "fg=${config.lib.stylix.colors.withHashtag.base0B-hex}:bold";
          false = "fg=${config.lib.stylix.colors.withHashtag.base08-hex}:bold";
          number = "fg=${config.lib.stylix.colors.withHashtag.base0E-hex}";
          null = "fg=${config.lib.stylix.colors.withHashtag.base04-hex}";
          quantity = "fg=${config.lib.stylix.colors.withHashtag.base0E-hex}";
          duration = "fg=${config.lib.stylix.colors.withHashtag.base09-hex}";
          durationfresh = "fg=${config.lib.stylix.colors.withHashtag.base0B-hex}";
          ratio = {
            zero = "fg=${config.lib.stylix.colors.withHashtag.base04-hex}";
            equal = "fg=${config.lib.stylix.colors.withHashtag.base0B-hex}";
            unequal = "fg=${config.lib.stylix.colors.withHashtag.base0A-hex}";
          };
        };
        status = {
          success = "fg=${config.lib.stylix.colors.withHashtag.base0B-hex}:bold";
          warning = "fg=${config.lib.stylix.colors.withHashtag.base0A-hex}:bold";
          error = "fg=${config.lib.stylix.colors.withHashtag.base08-hex}:bold";
        };
        table = {
          header = "fg=${config.lib.stylix.colors.withHashtag.base05-hex}:bold";
          columns = "fg=${config.lib.stylix.colors.withHashtag.base05-hex}";
        };
        stderr = {
          default = "fg=${config.lib.stylix.colors.withHashtag.base05-hex}";
          error = "fg=${config.lib.stylix.colors.withHashtag.base08-hex}:bold";
        };
        describe = {
          key = "fg=${config.lib.stylix.colors.withHashtag.base07-hex}:bold";
        };
        apply = {
          created = "fg=${config.lib.stylix.colors.withHashtag.base0B-hex}";
          configured = "fg=${config.lib.stylix.colors.withHashtag.base0A-hex}";
          unchanged = "fg=${config.lib.stylix.colors.withHashtag.base05-hex}";
          dryrun = "fg=${config.lib.stylix.colors.withHashtag.base0D-hex}";
          fallback = "fg=${config.lib.stylix.colors.withHashtag.base05-hex}";
        };
        explain = {
          key = "fg=${config.lib.stylix.colors.withHashtag.base07-hex}:bold";
          required = "fg=${config.lib.stylix.colors.withHashtag.base00-hex}:bold";
        };
        options = {
          flag = "fg=${config.lib.stylix.colors.withHashtag.base07-hex}:bold";
        };
        version = {
          key = "fg=${config.lib.stylix.colors.withHashtag.base07-hex}:bold";
        };
      };
    };
  };
}
