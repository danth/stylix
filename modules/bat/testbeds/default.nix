{ lib, pkgs, ... }:
let
  package = pkgs.bat;
in
{
  environment = {
    loginShellInit = "${lib.getExe package} example.md";
    systemPackages = [ package ];
  };
  home-manager.sharedModules = [
    {
      home.file."example.md" = {
        source = pkgs.fetchurl {
          url = "https://raw.githubusercontent.com/sharkdp/bat/refs/heads/master/tests/syntax-tests/source/Markdown/example.md";
          hash = "sha256-VYYwgRFY1c2DPY7yGM8oF3zG4rtEpBWyqfPwmGZIkcA=";
        };
      };
    }
  ];
}
