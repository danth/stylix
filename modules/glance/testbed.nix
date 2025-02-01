{ lib, pkgs, ... }:

let
  host = "127.0.0.1";
  port = 1234;
  package = pkgs.wrapFirefox pkgs.firefox-unwrapped {
    extraPolicies.OverrideFirstRunPage =
      "http://" + host + ":" + (builtins.toString port);
  };

in
{
  stylix.testbed.application = {
    enable = true;
    name = "firefox";
    inherit package;
  };

  home-manager.sharedModules = lib.singleton {
    programs.firefox = {
      enable = true;
      inherit package;
    };
    services.glance = {
      enable = true;
      settings = {
        server = {
          inherit host port;
        };
        pages = [
          {
            name = "Home";
            columns = [
              {
                size = "small";
                widgets = [
                  {
                    type = "weather";
                    units = "metric";
                    hour-format = "24h";
                    hide-location = false;
                    show-area-name = true;
                    location = "Tokyo, Japan";
                  }
                ];
              }
              {
                size = "full";
                widgets = [
                  {
                    type = "search";
                    autofocus = true;
                    search-engine = "https://github.com/NixOS/nixpkgs/pulls?q=is%3Apr+is%3Aopen+{QUERY}";
                  }
                  {
                    type = "group";
                    widgets = [

                      {
                        type = "rss";
                        style = "vertical-list";
                        collapse-after = 15;
                        feeds = [
                          {
                            url = "https://www.lesswrong.com/feed.xml?view=curated-rss";
                            title = "LessWrong";
                          }
                        ];
                      }
                    ];
                  }
                ];
              }
              {
                size = "small";
                widgets = [
                  { type = "calendar"; }
                ];
              }
            ];
          }
        ];
      };

    };
  };
}
