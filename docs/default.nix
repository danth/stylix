{
  pkgs,
  lib,
  inputs,
  ...
}:

let
  nixosConfiguration = lib.nixosSystem {
    inherit (pkgs) system;
    modules = [
      inputs.home-manager.nixosModules.home-manager
      inputs.self.nixosModules.stylix
      ./settings.nix
    ];
  };

  homeManagerConfiguration = inputs.home-manager.lib.homeManagerConfiguration {
    inherit pkgs;
    modules = [
      inputs.self.homeManagerModules.stylix
      ./settings.nix
      {
        home = {
          homeDirectory = "/home/book";
          stateVersion = "22.11";
          username = "book";
        };
      }
    ];
  };

  # TODO: Include Nix Darwin options

  platforms = {
    home_manager = {
      name = "Home Manager";
      configuration = homeManagerConfiguration;
    };
    nixos = {
      name = "NixOS";
      configuration = nixosConfiguration;
    };
  };

  insert =
    {
      index,
      page,
      emptyPage,
      platform,
      option,
    }:
    index
    // {
      ${page} =
        let
          oldPage = index.${page} or emptyPage;
        in
        oldPage
        // {
          optionsByPlatform = oldPage.optionsByPlatform // {
            ${platform} = oldPage.optionsByPlatform.${platform} ++ [ option ];
          };
        };
    };

  insertDeclaration =
    {
      index,
      declaration,
      platform,
      option,
    }:
    if lib.hasPrefix "${inputs.self}/" declaration then
      let
        path = lib.removePrefix "${inputs.self}/" declaration;
        pathComponents = lib.splitString "/" path;
      in
      if builtins.elemAt pathComponents 0 == "modules" then
        let
          module = builtins.unsafeDiscardStringContext (builtins.elemAt pathComponents 1);
        in
        insert {
          inherit index platform option;
          page = "src/options/modules/${module}.md";
          emptyPage = {
            referenceSection = "Modules";
            readme = "${inputs.self}/modules/${module}/README.md";
            defaultReadme = ''
              # ${module}
              > [!NOTE]
              > This module doesn't include any additional documentation.
              > You can browse the options it provides below.
            '';
            # Modules pages initialise all platforms to an empty list, so that
            # *None provided.* indicates where the module isn't available.
            optionsByPlatform = lib.mapAttrs (_: _: [ ]) platforms;
          };
        }
      else
        insert {
          inherit index platform option;
          page = "src/options/platforms/${platform}.md";
          emptyPage = {
            referenceSection = "Platforms";
            readme = "${inputs.self}/docs/src/options/platforms/${platform}.md";
            defaultReadme = ''
              # ${platform.name}
              > Documentation is not available for this platform. Its main
              > options are listed below, and you may find more specific
              > options in the documentation for each module.
            '';
            # Platform pages only initialise that platform, since showing other
            # platforms here would be nonsensical.
            optionsByPlatform = {
              ${platform} = [ ];
            };
          };
        }
    else
      index;

  insertOption =
    {
      index,
      platform,
      option,
    }:
    builtins.foldl' (
      foldIndex: declaration:
      insertDeclaration {
        index = foldIndex;
        inherit declaration platform option;
      }
    ) index option.declarations;

  insertPlatform =
    index: platform:
    builtins.foldl'
      (
        foldIndex: option:
        insertOption {
          index = foldIndex;
          inherit platform option;
        }
      )
      index
      (lib.optionAttrSetToDocList platforms.${platform}.configuration.options);

  index = builtins.foldl' insertPlatform { } (builtins.attrNames platforms);

  # You can embed HTML inside a Markdown document, but to render further
  # Markdown within that HTML, it must be surrounded by blank lines.
  # This function helps with that.
  #
  # In other functions, we use concatStrings to build embedded HTML, rather
  # than multiline strings, because Markdown is sensitive to indentation and
  # may render indented HTML as a code block.
  markdownInHTML = markdown: "\n\n" + markdown + "\n\n";

  renderDetailsRow =
    name: value:
    lib.concatStrings [
      "<tr>"
      "<td>"
      (markdownInHTML name)
      "</td>"
      "<td>"
      (markdownInHTML value)
      "</td>"
      "</tr>"
    ];

  renderValue =
    value:
    if lib.isType "literalMD" value then
      value.text
    else if lib.isType "literalExpression" value then
      ''
        ```nix
        ${value.text}
        ```
      ''
    else
      builtins.throw "no implementation for rendering this kind of value";

  # Prefix to remove from file paths when listing where an option is declared.
  declarationPrefix = "${inputs.self}";

  # Permalink to view the declaration on GitHub. If the commit isn't known,
  # then fall back to the latest commit.
  declarationCommit = inputs.self.rev or "master";
  declarationPermalink = "https://github.com/danth/stylix/blob/${declarationCommit}";

  renderDeclaration =
    declaration:
    let
      declarationString = toString declaration;
      filePath = lib.removePrefix "${declarationPrefix}/" declarationString;
    in
    if lib.hasPrefix declarationPrefix declarationString then
      "- [${filePath}](${declarationPermalink}/${filePath})"
    else
      builtins.throw "declaration not in ${declarationPrefix}: ${declarationString}";

  renderOption =
    option:
    lib.optionalString (option.visible && !option.internal) ''
      ### ${option.name}

      ${lib.concatStrings (
        [
          "<table class=\"option-details\">"
          "<colgroup>"
          "<col span=\"1\">"
          "<col span=\"1\">"
          "</colgroup>"
          "<tbody>"
        ]
        ++ (lib.optional (option ? description) (
          renderDetailsRow "Summary" option.description
        ))
        ++ (lib.optional (option ? type) (renderDetailsRow "Type" option.type))
        ++ (lib.optional (option ? default) (
          renderDetailsRow "Default" (renderValue option.default)
        ))
        ++ (lib.optional (option ? example) (
          renderDetailsRow "Example" (renderValue option.example)
        ))
        ++ (lib.optional (option ? declarations) (
          renderDetailsRow "Source" (
            lib.concatLines (map renderDeclaration option.declarations)
          )
        ))
        ++ [
          "</tbody>"
          "</table>"
        ]
      )}
    '';

  renderPlatform =
    platform: options:
    let
      sortedOptions = builtins.sort (a: b: a.name < b.name) options;
      renderedOptions =
        if sortedOptions == [ ] then
          "*None provided.*"
        else
          lib.concatLines (map renderOption sortedOptions);
    in
    ''
      ## ${platforms.${platform}.name} options
      ${renderedOptions}
    '';

  renderPage =
    _path: page:
    let
      readme =
        # This doesn't count as IFD because ${inputs.self} is a flake input
        if builtins.pathExists page.readme then
          builtins.readFile page.readme
        else
          page.defaultReadme;
      options = lib.concatStrings (
        lib.mapAttrsToList renderPlatform page.optionsByPlatform
      );
    in
    lib.concatLines [
      readme
      options
    ];

  renderedPages = lib.mapAttrs renderPage index;

  insertPageSummary =
    summary: path: page:
    let
      text = renderedPages.${path};
      lines = lib.splitString "\n" text;
      firstLine = builtins.elemAt lines 0;
      title = lib.removePrefix "# " firstLine;
      relativePath = lib.removePrefix "src/" path;
      entry =
        if title == firstLine then
          builtins.throw "${path} must begin with a title"
        else
          "  - [${title}](${relativePath})";
    in
    summary
    // {
      ${page.referenceSection} = (summary.${page.referenceSection} or [ ]) ++ [
        entry
      ];
    };

  summary = lib.foldlAttrs insertPageSummary { } index;

  renderSummarySection =
    referenceSection: entries:
    let
      parentEntry = "- [${referenceSection}]()";
    in
    [ parentEntry ] ++ entries;

  renderedSummary = lib.concatLines (
    lib.flatten (lib.mapAttrsToList renderSummarySection summary)
  );

  writePages = lib.concatLines (
    lib.mapAttrsToList (
      path: text:
      let
        file = pkgs.writeText path text;
      in
      "install -D ${file} ${path}"
    ) renderedPages
  );

  # Some adjustments for a more uniform and compact layout while using a
  # separate table for each option.
  extraCSS = ''
    .option-details {
      width: 100%;
      table-layout: fixed;
    }
    .option-details col:first-child {
      width: 7.5em;
    }
    .option-details col:last-child {
      width: 100%;
      overflow-x: auto;
    }
    .option-details tr {
      background: inherit !important;
    }
    .option-details ol, .option-details ul {
      list-style: none;
      padding: unset;
    }
  '';

in
pkgs.stdenvNoCC.mkDerivation {
  name = "stylix-book";
  src = ./.;
  buildInputs = with pkgs; [
    mdbook
    mdbook-alerts
  ];

  patchPhase = ''
    ${writePages}
    echo -n ${lib.escapeShellArg renderedSummary} >>src/SUMMARY.md
    cp ${../README.md} src/README.md
    cp ${../gnome.png} src/gnome.png
    cp ${../kde.png} src/kde.png
  '';

  buildPhase = "mdbook build --dest-dir $out";

  fixupPhase = ''
    echo ${lib.escapeShellArg extraCSS} >>$out/css/general.css
  '';
}
