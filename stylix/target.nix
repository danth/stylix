{ config, lib, ... }:

{
  options.stylix = {
    enable = lib.mkOption {
      description = ''
        Whether to enable Stylix.

        When this is `false`, all theming is disabled and all other options
        are ignored.
      '';
      type = lib.types.bool;
      default = false;
      example = true;
    };

    autoEnable = lib.mkOption {
      description = ''
        Whether to enable targets by default.

        When this is `false`, all targets are disabled unless explicitly enabled.

        When this is `true`, most targets are enabled by default. A small number
        remain off by default, because they require further manual setup, or
        they are only applicable in specific circumstances which cannot be
        detected automatically.
      '';
      type = lib.types.bool;
      default = true;
      example = false;
    };
  };

  config.lib.stylix =
    let
      cfg = config.stylix;
      self = config.lib.stylix;

      # Will wrap with (parentheses) if the expr contains operators with lower precedence than `&&`
      wrapExprWith =
        {
          autoWrapExpr ? true,
          trimExpr ? true,
          indentMultilineExpr ? true,
        }:
        expr:
        let
          trimmed = if trimExpr then lib.trim expr else expr;
          isWrapped = builtins.match ''[(].*[)]'' trimmed != null;
          hasNewlines = lib.hasInfix "\n" trimmed;
          needsWrapping = builtins.any (op: lib.hasInfix op trimmed) [
            # These operators have lower precedence than `&&`
            # See https://nix.dev/manual/nix/2.28/language/operators
            "||"
            "->"
            "|>"
            "<|"
            # These keywords would also need wrapping
            "with "
            "assert "
          ];
          indented =
            if indentMultilineExpr then
              lib.pipe trimmed [
                (lib.strings.splitString "\n")
                (map (line: if line == "" then "" else "  " + line))
                (builtins.concatStringsSep "\n")
              ]
            else
              trimmed;
          wrapped = if hasNewlines then "(\n${indented}\n)" else "(${trimmed})";
        in
        if autoWrapExpr && !isWrapped && needsWrapping then wrapped else trimmed;
    in
    {
      mkEnableTarget =
        name: autoEnable:
        config.lib.stylix.mkEnableTargetWith { inherit name autoEnable; };

      mkEnableTargetWith =
        {
          name,
          autoEnable ? true,
          autoEnableExpr ? null,
          autoWrapExpr ? true,
          example ? if args ? autoEnableExpr then true else !autoEnable,
        }@args:
        let
          wrapExpr = wrapExprWith {
            inherit autoWrapExpr;
          };
        in
        self.mkEnableIf {
          description = "Whether to enable theming for ${name}";
          default = cfg.autoEnable && autoEnable;
          defaultText =
            if args ? autoEnableExpr then
              lib.literalExpression "stylix.autoEnable && ${wrapExpr autoEnableExpr}"
            else if autoEnable then
              lib.literalExpression "stylix.autoEnable"
            else
              false;
          inherit example;
        };

      mkEnableWallpaper =
        humanName: autoEnable:
        self.mkEnableIf {
          description = "Whether to set the wallpaper for ${humanName}.";
          default = config.stylix.image != null && autoEnable;
          defaultText =
            if autoEnable then lib.literalExpression "stylix.image != null" else false;
          example = config.stylix.image == null;
        };

      mkEnableIf =
        {
          description,
          default,
          defaultText ? null,
          example ? if args ? defaultText then true else !default,
        }@args:
        lib.mkOption {
          type = lib.types.bool;
          defaultText = if args ? defaultText then defaultText else default;
          inherit
            default
            description
            example
            ;
        };
    };
}
