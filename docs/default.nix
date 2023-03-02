{ pkgs, pkgsLib, coricamuLib, config, inputs, ... }:

with coricamuLib;

rec {
  baseUrl = "https://danth.github.io/stylix/";
  siteTitle = "Stylix";
  language = "en-gb";

  header = makeProjectHeader {
    title = siteTitle;
    inherit (config) pages;
    repository = "https://github.com/danth/stylix";
  };

  pages = makeProjectPages ../. ++ [
    {
      path = "options.html";
      title = "NixOS options";

      body.docbook =
        let
          configuration = pkgsLib.nixosSystem {
            inherit (pkgs) system;
            modules = [
              inputs.self.nixosModules.stylix
              ./settings.nix
              { _module.check = false; }
            ];
          };
        in
          builtins.readFile ./nixos_header.xml +
          makeOptionsDocBook {
            inherit (configuration) options;
            customFilter = option: builtins.elemAt option.loc 0 == "stylix";
          };
    }

    {
      path = "options-hm.html";
      title = "Home Manager options";

      body.docbook =
        let
          configuration = inputs.home-manager.lib.homeManagerConfiguration {
            inherit pkgs;
            modules = [
              inputs.self.homeManagerModules.stylix
              ./settings.nix
              {
                home = {
                  homeDirectory = "/home/docs";
                  stateVersion = "22.11";
                  username = "docs";
                };
              }
            ];
            check = false;
          };
        in
          builtins.readFile ./hm_header.xml +
          makeOptionsDocBook {
            inherit (configuration) options;
            customFilter = option: builtins.elemAt option.loc 0 == "stylix";
          };
    }
  ];
}
