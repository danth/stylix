{ lib }:

# string -> [ path ]
# List include path for either nixos modules or hm modules
platform:
builtins.concatLists (
  lib.mapAttrsToList (
    target: kind:
    let
      file = ../modules/${target}/${platform}.nix;
      module = import file;

      # Detect whether the file's value has an argument named `mkTarget`
      useMkTarget =
        builtins.isFunction module && (builtins.functionArgs module) ? mkTarget;

      # NOTE: `mkTarget` cannot be distributed normally through the module system
      # due to issues of infinite recursion.
      mkTarget = import ./mk-target.nix;
    in
    lib.optional (kind == "directory" && builtins.pathExists file) (
      if useMkTarget then
        { config, ... }@args:
        let
          # Based on `lib.modules.applyModuleArgs`
          #
          # Apply `mkTarget` as a special arg without actually using `specialArgs`,
          # which cannot be defined from within a configuration.
          context =
            name: ''while evaluating the module argument `${name}' in "${toString file}":'';
          extraArgs = lib.pipe module [
            builtins.functionArgs
            (lib.flip builtins.removeAttrs [ "mkTarget" ])
            (builtins.mapAttrs (
              name: _:
              builtins.addErrorContext (context name) (
                args.${name} or config._module.args.${name}
              )
            ))
          ];
        in
        {
          key = file;
          _file = file;
          imports = [ (module (args // extraArgs // { inherit mkTarget; })) ];
        }
      else
        file
    )
  ) (builtins.readDir ../modules)
)
