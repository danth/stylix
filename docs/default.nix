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

  # We construct an index of all Stylix options, using the following format:
  #
  #     {
  #       "src/options/modules/«module».md" = {
  #         referenceSection = "Modules";
  #         readme = "modules/«module»/README.md";
  #         defaultReadme = "note about the path above not existing";
  #         optionsByPlatform = {
  #           home_manager = [ ... ];
  #           nixos = [ ... ];
  #         };
  #       };
  #
  #       "src/options/platforms/«platform».md" = {
  #         referenceSection = "Platforms";
  #         readme = "docs/src/options/platforms/«platform».md";
  #         defaultReadme = "note about the path above not existing";
  #         optionsByPlatform.«platform» = [ ... ];
  #       };
  #     }
  #
  # Options are inserted one at a time into the appropriate page, creating
  # new page entries if they don't exist.

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
    # Only include options which are declared by a module within Stylix.
    if lib.hasPrefix "${inputs.self}/" declaration then
      let
        # Part of this string may become an attribute name in the index, and
        # attribute names aren't allowed to have string context. The context
        # comes from `${inputs.self}`, which is removed by `removePrefix`.
        # Therefore, this use of `unsafeDiscardStringContext` is safe.
        pathWithContext = lib.removePrefix "${inputs.self}/" declaration;
        path = builtins.unsafeDiscardStringContext pathWithContext;
        pathComponents = lib.splitString "/" path;
      in
      # Options declared in the modules directory go to the Modules section,
      # otherwise they're assumed to be shared between modules, and go to the
      # Platforms section.
      if builtins.elemAt pathComponents 0 == "modules" then
        let
          module = builtins.elemAt pathComponents 1;
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
              > This module doesn't include any additional documentation. You
              > can browse the options it provides below.
            '';
            # Module pages initialise all platforms to an empty list, so that
            # '*None provided.*' indicates platforms where the module isn't
            # available.
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
              > options are listed below, and you may find more specific options
              > in the documentation for each module.
            '';
            # Platform pages only initialise that platform, since showing other
            # platforms here would be nonsensical.
            optionsByPlatform.${platform} = [ ];
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

  # Renders a value, which should have been created with either lib.literalMD
  # or lib.literalExpression.
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
      builtins.throw "unexpected value type: ${builtins.typeOf value}";

  # Prefix to remove from file paths when listing where an option is declared.
  declarationPrefix = "${inputs.self}";

  # Permalink to view a source file on GitHub. If the commit isn't known,
  # then fall back to the latest commit.
  declarationCommit = inputs.self.rev or "master";
  declarationPermalink = "https://github.com/danth/stylix/blob/${declarationCommit}";

  # Renders a single option declaration. Example output:
  #
  # - [modules/module1/nixos.nix](https://github.com/danth/stylix/blob/«commit»/modules/module1/nixos.nix)
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

  # You can embed HTML inside a Markdown document, but to render further
  # Markdown within that HTML, it must be surrounded by blank lines.
  # This function helps with that.
  #
  # In the following functions, we use concatStrings to build embedded HTML,
  # rather than ${} and multiline strings, because Markdown is sensitive to
  # indentation and may render indented HTML as a code block. The easiest way
  # around this is to generate all the HTML on a single line.
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

  # Render a single option. Example output (actually HTML, but drawn here using
  # pseudo-Markdown for clarity):
  #
  #     ### stylix.option.one
  #
  #     The option's description, if present.
  #
  #     | Type    | string                                                |
  #     | Default | The default value, if provided. Usually a code block. |
  #     | Example | An example value, if provided. Usually a code block.  |
  #     | Source  | - [modules/module1/nixos.nix](https://github.com/...) |
  renderOption =
    option:
    lib.optionalString (option.visible && !option.internal) ''
      ### ${option.name}

      ${option.description or ""}

      ${lib.concatStrings (
        [
          "<table class=\"option-details\">"
          "<colgroup>"
          "<col span=\"1\">"
          "<col span=\"1\">"
          "</colgroup>"
          "<tbody>"
        ]
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

  # Render the list of options for a single platform. Example output:
  #
  #     ## NixOS options
  #     ### stylix.option.one
  #     «option details»
  #     ### stylix.option.two
  #     «option details»
  renderPlatform =
    platform: options:
    let
      sortedOptions = builtins.sort (a: b: a.name < b.name) options;
      renderedOptions =
        if options == [ ] then
          "*None provided.*"
        else
          lib.concatLines (map renderOption sortedOptions);
    in
    ''
      ## ${platforms.${platform}.name} options
      ${renderedOptions}
    '';

  # Renders the list of options for all platforms on a page, preceded by either
  # the relevant README, or the default README if it doesn't exist.
  #
  # Example output:
  #
  #     # Module 1
  #
  #     This is the content of `modules/module1/README.md`, including the title
  #     above.
  #
  #     ## Home Manager options
  #     *None provided.*
  #
  #     ## NixOS options
  #     «list of options»
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

  # SUMMARY.md is generated by a similar method to the main index, using
  # the following format:
  #
  #     {
  #       Modules = [
  #         "  - [Module 1](src/options/modules/module1.md)"
  #         "  - [Module 2](src/options/modules/module2.md)"
  #       ];
  #       Platforms = [
  #         "  - [Home Manager](src/options/platforms/home_manager.md)"
  #         "  - [NixOS](src/options/platforms/nixos.md)"
  #       ];
  #     }
  #
  # Which renders to the following:
  #
  #     - [Modules]()
  #       - [Module 1](src/options/modules/module1.md)
  #       - [Module 2](src/options/modules/module2.md)
  #     - [Platforms]()
  #       - [Home Manager](src/options/platforms/home_manager.md)
  #       - [NixOS](src/options/platforms/nixos.md)
  #
  # In mdbook, an empty link denotes a draft page, which is used as a parent to
  # collapse the section in the sidebar.

  insertPageSummary =
    summary: path: page:
    let
      # Extract the title from the first line of the page, and use it in the
      # summary. This ensures that page titles match the sidebar, and ensures
      # that each page begins with a title.
      #
      # TODO: There's potential to use the title from platform pages as the
      # subheading for that platform on other pages, rather than defining a
      # name in the `platforms` attribute set earlier in this file.
      # (This is likely wasted effort unless we have a reason to add a large
      #  number of platforms.)
      text = renderedPages.${path};
      lines = lib.splitString "\n" text;
      firstLine = builtins.elemAt lines 0;
      titlePrefix = "# ";
      hasTitle = lib.hasPrefix titlePrefix firstLine;
      title = lib.removePrefix titlePrefix firstLine;
      relativePath = lib.removePrefix "src/" path;
      entry =
        if hasTitle then
          "  - [${title}](${relativePath})"
        else
          builtins.throw "page must start with a title: ${path}";
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
      # In mdbook, an empty link denotes a draft page, which is used as a
      # parent so the section can be collapsed in the sidebar.
      parentEntry = "- [${referenceSection}]()";
    in
    [ parentEntry ] ++ entries;

  renderedSummary = lib.concatLines (
    lib.flatten (lib.mapAttrsToList renderSummarySection summary)
  );

  # This function generates a Bash script that installs each page to the
  # correct location, over the top of an original copy of docs/src.
  #
  # Each page must be written in a separate derivation, because passing all
  # the text into a single derivation exceeds the maximum size of command
  # line arguments.
  #
  # TODO: It should be possible to use symlinkJoin here, which would make the
  # code more robust at the expense of another intermediate derivation.
  # However, that derivation would be useful during development for inspecting
  # the Markdown before it's rendered to HTML.
  writePages = lib.concatLines (
    lib.mapAttrsToList (
      path: text:
      let
        file = pkgs.writeText path text;
      in
      "install -D ${file} ${path}"
    ) renderedPages
  );

  # Every option has a separate table containing its details. This CSS makes
  # the following changes for better consistency and compactness:
  #
  # - Fix the width of tables and their columns, so the layout is consistent
  #   when scanning through the options. By default, tables are centered and
  #   sized to their individual content.
  # - Remove the alternating background colour from rows, which is distracting
  #   when there is a small number of rows with a potentially large amount
  #   of text per row.
  # - Allow text within a cell to scroll horizontally, which is useful for
  #   wide code blocks, especially on mobile devices.
  # - Remove bullet points from lists; this is intended for the list of
  #   declarations, as it often contains only one item. Again, this is aimed
  #   at mobile devices where horizontal space is limited.
  #   TODO: Constrain this rule to only apply to the declarations list, as it
  #   may interfere with option descriptions that contain lists.
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

  inherit extraCSS renderedSummary;
  passAsFile = [
    "extraCSS"
    "renderedSummary"
  ];

  patchPhase = ''
    ${writePages}
    cat $renderedSummaryPath >>src/SUMMARY.md
    cp ${../README.md} src/README.md
    cp ${../gnome.png} src/gnome.png
    cp ${../kde.png} src/kde.png
  '';

  buildPhase = ''
    runHook preBuild
    mdbook build --dest-dir $out
    runHook postBuild
  '';

  postBuild = ''
    cat $extraCSSPath >>$out/css/general.css
  '';
}
