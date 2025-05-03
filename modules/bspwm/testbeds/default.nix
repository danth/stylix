{ pkgs, ... }:

{
  config = {
    environment.systemPackages = [ pkgs.kitty ];

    stylix.testbed.ui = {
      graphicalEnvironment = "bspwm";

      # We need something to open a window so that we can check the window borders
      application = {
        name = "kitty";
        package = pkgs.kitty;
      };
    };
  };
}
