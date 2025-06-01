{ inputs, ... }:
{
  imports = [
    inputs.git-hooks.flakeModule
  ];

  perSystem =
    { config, ... }:
    {
      pre-commit = {
        check.enable = true;

        settings.hooks = {
          deadnix.enable = true;
          editorconfig-checker.enable = true;
          hlint.enable = true;

          treefmt = {
            enable = true;
            package = config.formatter;
          };

          statix.enable = true;
          typos = {
            enable = true;
            settings.configuration = ''
              [default.extend-identifiers]
              MrSom3body="MrSom3body"
            '';
          };
          yamllint.enable = true;
        };
      };
    };
}
