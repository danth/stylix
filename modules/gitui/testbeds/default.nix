{
  pkgs,
  lib,
  ...
}:
let
  package = pkgs.symlinkJoin {
    name = "gitui";
    paths = [
      pkgs.gitui
      (pkgs.makeDesktopItem {
        name = "gitui";
        exec = "kgx -e \"gitui --directory ${inputs.self}\"";
        desktopName = "gitui";
        categories = [ "Development" ];
      })
    ];
  };
in
{
  stylix.testbed.application = {
    enable = true;
    name = "gitui";
    inherit package;
  };

  home-manager.sharedModules = lib.singleton {
    programs.gitui = {
      enable = true;
      inherit package;
    };
  };

  environment.systemPackages = [
    package
    pkgs.git
  ];
}
