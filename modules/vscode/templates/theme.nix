{ config, ... }:

with config.lib.stylix.colors.withHashtag;

{
  "$schema" = "vscode://schemas/color-theme";
  name = scheme;
  type = "dark";
  colors = {
    "contrastActiveBorder" = null;
    "contrastBorder" = null;
    "focusBorder" = base0D;
    "foreground" = base05;
    "disabledForeground" = base04;
    "widget.border" = base02;
    "widget.shadow" = "#00000000";
    "selection.background" = base02;
    "descriptionForeground" = "${base05}99";
    "errorForeground" = base08;
    "icon.foreground" = base05;
    "sash.hoverBorder" = null;
    "window.activeBorder" = null;
    "window.inactiveBorder" = null;
    "textBlockQuote.background" = base01;
    "textBlockQuote.border" = base0D;
    "textCodeBlock.background" = base00;
    "textLink.activeForeground" = base0C;
    "textLink.foreground" = base0D;
    "textPreformat.foreground" = base0D;
    "textPreformat.background" = null;
    "textSeparator.foreground" = base05;
    "toolbar.hoverBackground" = base02;
    "toolbar.hoverOutline" = null;
    "toolbar.activeBackground" = base02;
    "button.background" = base0D;
    "button.foreground" = base00;
    "button.border" = null;
    "button.separator" = null;
    "button.hoverBackground" = "${base0D}C0";
    "button.secondaryForeground" = base00;
    "button.secondaryBackground" = base0E;
    "button.secondaryHoverBackground" = "${base0E}C0";
    "checkbox.background" = base00;
    "checkbox.foreground" = base05;
    "checkbox.border" = null;
    "checkbox.selectBackground" = null;
    "checkbox.selectBorder" = null;
    "dropdown.background" = base00;
    "dropdown.listBackground" = base00;
    "dropdown.border" = null;
    "dropdown.foreground" = base05;
    "input.background" = base00;
    "input.border" = null;
    "input.foreground" = base05;
    "input.placeholderForeground" = base03;
    "inputOption.activeBackground" = base0D;
    "inputOption.activeBorder" = null;
    "inputOption.activeForeground" = base00;
    "inputOption.hoverBackground" = null;
    "inputValidation.errorBackground" = base08;
    "inputValidation.errorForeground" = base05;
    "inputValidation.errorBorder" = base08;
    "inputValidation.infoBackground" = base0D;
    "inputValidation.infoForeground" = base05;
    "inputValidation.infoBorder" = base0D;
    "inputValidation.warningBackground" = base0A;
    "inputValidation.warningForeground" = base05;
    "inputValidation.warningBorder" = base0A;
    "scrollbar.shadow" = "#00000000";
    "scrollbarSlider.activeBackground" = "${base04}77";
    "scrollbarSlider.background" = "${base03}33";
    "scrollbarSlider.hoverBackground" = "${base03}77";
    "badge.foreground" = base05;
    "badge.background" = base00;
    "progressBar.background" = base03;
    "list.activeSelectionBackground" = base02;
    "list.activeSelectionForeground" = base05;
    "list.activeSelectionIconForeground" = null;
    "list.dropBackground" = "${base03}66";
    "list.focusBackground" = base02;
    "list.focusForeground" = base05;
    "list.focusHighlightForeground" = null;
    "list.focusOutline" = base0D;
    "list.focusAndSelectionOutline" = base0D;
    "list.highlightForeground" = base07;
    "list.hoverBackground" = base02;
    "list.hoverForeground" = base05;
    "list.inactiveSelectionBackground" = base02;
    "list.inactiveSelectionForeground" = base05;
    "list.inactiveSelectionIconForeground" = null;
    "list.inactiveFocusBackground" = base02;
    "list.inactiveFocusOutline" = base03;
    "list.invalidItemForeground" = base08;
    "list.errorForeground" = base08;
    "list.warningForeground" = base0A;
    "listFilterWidget.background" = base00;
    "listFilterWidget.outline" = null;
    "listFilterWidget.noMatchesOutline" = base08;
    "listFilterWidget.shadow" = "#00000000";
    "list.filterMatchBackground" = base02;
    "list.filterMatchBorder" = null;
    "list.deemphasizedForeground" = null;
    "list.dropBetweenBackground" = null;
    "tree.indentGuidesStroke" = base05;
    "tree.inactiveIndentGuidesStroke" = null;
    "tree.tableColumnsBorder" = null;
    "tree.tableOddRowsBackground" = null;
    "activityBar.background" = base01;
    "activityBar.dropBorder" = base03;
    "activityBar.foreground" = base05;
    "activityBar.inactiveForeground" = base03;
    "activityBar.border" = null;
    "activityBarBadge.background" = base0D;
    "activityBarBadge.foreground" = base00;
    "activityBar.activeBorder" = base05;
    "activityBar.activeBackground" = base02;
    "activityBar.activeFocusBorder" = base0D;
    "activityBarTop.foreground" = base05;
    "activityBarTop.activeBorder" = base0D;
    "activityBarTop.inactiveForeground" = base03;
    "activityBarTop.dropBorder" = base03;
    "profileBadge.background" = base01;
    "profileBadge.foreground" = base03;
    "sideBar.background" = base01;
    "sideBar.foreground" = base05;
    "sideBar.border" = null;
    "sideBar.dropBackground" = base02;
    "sideBarTitle.foreground" = base05;
    "sideBarSectionHeader.background" = base01;
    "sideBarSectionHeader.foreground" = base05;
    "sideBarSectionHeader.border" = null;
    "minimap.findMatchHighlight" = base0A;
    "minimap.selectionHighlight" = base02;
    "minimap.errorHighlight" = base08;
    "minimap.warningHighlight" = base0A;
    "minimap.background" = base00;
    "minimap.selectionOccurrenceHighlight" = base03;
    "minimap.foregroundOpacity" = null;
    "minimap.infoHighlight" = null;
    "minimapSlider.background" = null;
    "minimapSlider.hoverBackground" = null;
    "minimapSlider.activeBackground" = null;
    "minimapGutter.addedBackground" = base0B;
    "minimapGutter.modifiedBackground" = base0E;
    "minimapGutter.deletedBackground" = base08;
    "editorGroup.border" = null;
    "editorGroup.dropBackground" = "${base03}66";
    "editorGroupHeader.noTabsBackground" = base01;
    "editorGroupHeader.tabsBackground" = base01;
    "editorGroupHeader.tabsBorder" = null;
    "editorGroupHeader.border" = null;
    "editorGroup.emptyBackground" = base00;
    "editorGroup.focusedEmptyBorder" = base0D;
    "editorGroup.dropIntoPromptForeground" = base06;
    "editorGroup.dropIntoPromptBackground" = base00;
    "editorGroup.dropIntoPromptBorder" = null;
    "tab.activeBackground" = base02;
    "tab.unfocusedActiveBackground" = base02;
    "tab.activeForeground" = base05;
    "tab.border" = "#00000000";
    "tab.activeBorder" = null;
    "tab.dragAndDropBorder" = base03;
    "tab.unfocusedActiveBorder" = null;
    "tab.activeBorderTop" = null;
    "tab.unfocusedActiveBorderTop" = null;
    "tab.lastPinnedBorder" = null;
    "tab.inactiveBackground" = base01;
    "tab.unfocusedInactiveBackground" = base01;
    "tab.inactiveForeground" = base05;
    "tab.unfocusedActiveForeground" = base04;
    "tab.unfocusedInactiveForeground" = base04;
    "tab.hoverBackground" = base02;
    "tab.unfocusedHoverBackground" = base02;
    "tab.hoverForeground" = base05;
    "tab.unfocusedHoverForeground" = base05;
    "tab.hoverBorder" = null;
    "tab.unfocusedHoverBorder" = null;
    "tab.activeModifiedBorder" = base0D;
    "tab.inactiveModifiedBorder" = base0D;
    "tab.unfocusedActiveModifiedBorder" = base0D;
    "tab.unfocusedInactiveModifiedBorder" = base0D;
    "editorPane.background" = base00;
    "sideBySideEditor.horizontalBorder" = null;
    "sideBySideEditor.verticalBorder" = null;
    "editor.background" = base00;
    "editor.foreground" = base05;
    "editorLineNumber.foreground" = base03;
    "editorLineNumber.activeForeground" = base04;
    "editorLineNumber.dimmedForeground" = null;
    "editorCursor.background" = null;
    "editorCursor.foreground" = base05;
    "editor.selectionBackground" = base02;
    "editor.selectionForeground" = null;
    "editor.inactiveSelectionBackground" = base02;
    "editor.selectionHighlightBackground" = base01;
    "editor.selectionHighlightBorder" = null;
    "editor.wordHighlightBackground" = base02;
    "editor.wordHighlightBorder" = null;
    "editor.wordHighlightStrongBackground" = base03;
    "editor.wordHighlightStrongBorder" = null;
    "editor.wordHighlightTextBackground" = null;
    "editor.wordHighlightTextBorder" = null;
    "editor.findMatchBackground" = "${base0A}18";
    "editor.findMatchHighlightBackground" = "${base0A}66";
    "editor.findRangeHighlightBackground" = base01;
    "editor.findMatchBorder" = base0A;
    "editor.findMatchHighlightBorder" = null;
    "editor.findRangeHighlightBorder" = null;
    "search.resultsInfoForeground" = null;
    "searchEditor.findMatchBackground" = "${base0A}99";
    "searchEditor.findMatchBorder" = null;
    "searchEditor.textInputBorder" = null;
    "editor.hoverHighlightBackground" = base02;
    "editor.lineHighlightBackground" = base01;
    "editor.lineHighlightBorder" = null;
    "editorWatermark.foreground" = null;
    "editorUnicodeHighlight.border" = null;
    "editorUnicodeHighlight.background" = null;
    "editorLink.activeForeground" = base0D;
    "editor.rangeHighlightBackground" = base01;
    "editor.rangeHighlightBorder" = null;
    "editor.symbolHighlightBackground" = null;
    "editor.symbolHighlightBorder" = null;
    "editorWhitespace.foreground" = base03;
    "editorIndentGuide.background" = base02;
    "editorIndentGuide.background1" = null;
    "editorIndentGuide.background2" = null;
    "editorIndentGuide.background3" = null;
    "editorIndentGuide.background4" = null;
    "editorIndentGuide.background5" = null;
    "editorIndentGuide.background6" = null;
    "editorIndentGuide.activeBackground" = base02;
    "editorIndentGuide.activeBackground1" = null;
    "editorIndentGuide.activeBackground2" = null;
    "editorIndentGuide.activeBackground3" = null;
    "editorIndentGuide.activeBackground4" = null;
    "editorIndentGuide.activeBackground5" = null;
    "editorIndentGuide.activeBackground6" = null;
    "editorInlayHint.background" = base01;
    "editorInlayHint.foreground" = base03;
    "editorInlayHint.typeForeground" = base03;
    "editorInlayHint.typeBackground" = base01;
    "editorInlayHint.parameterForeground" = base03;
    "editorInlayHint.parameterBackground" = base01;
    "editorRuler.foreground" = base02;
    "editor.linkedEditingBackground" = null;
    "editorCodeLens.foreground" = base02;
    "editorLightBulb.foreground" = base0A;
    "editorLightBulbAutoFix.foreground" = base0D;
    "editorLightBulbAi.foreground" = null;
    "editorBracketMatch.background" = base02;
    "editorBracketMatch.border" = null;
    "editorBracketHighlight.foreground1" = base08;
    "editorBracketHighlight.foreground2" = base09;
    "editorBracketHighlight.foreground3" = base0A;
    "editorBracketHighlight.foreground4" = base0B;
    "editorBracketHighlight.foreground5" = base0D;
    "editorBracketHighlight.foreground6" = base0E;
    "editorBracketHighlight.unexpectedBracket.foreground" = base0F;
    "editorBracketPairGuide.activeBackground1" = null;
    "editorBracketPairGuide.activeBackground2" = null;
    "editorBracketPairGuide.activeBackground3" = null;
    "editorBracketPairGuide.activeBackground4" = null;
    "editorBracketPairGuide.activeBackground5" = null;
    "editorBracketPairGuide.activeBackground6" = null;
    "editorBracketPairGuide.background1" = null;
    "editorBracketPairGuide.background2" = null;
    "editorBracketPairGuide.background3" = null;
    "editorBracketPairGuide.background4" = null;
    "editorBracketPairGuide.background5" = null;
    "editorBracketPairGuide.background6" = null;
    "editor.foldBackground" = null;
    "editorOverviewRuler.background" = null;
    "editorOverviewRuler.border" = "#00000000";
    "editorOverviewRuler.findMatchForeground" = base0A;
    "editorOverviewRuler.rangeHighlightForeground" = base03;
    "editorOverviewRuler.selectionHighlightForeground" = base02;
    "editorOverviewRuler.wordHighlightForeground" = base07;
    "editorOverviewRuler.wordHighlightStrongForeground" = base0D;
    "editorOverviewRuler.wordHighlightTextForeground" = null;
    "editorOverviewRuler.modifiedForeground" = base0E;
    "editorOverviewRuler.addedForeground" = base0B;
    "editorOverviewRuler.deletedForeground" = base08;
    "editorOverviewRuler.errorForeground" = base08;
    "editorOverviewRuler.warningForeground" = base0A;
    "editorOverviewRuler.infoForeground" = base0C;
    "editorOverviewRuler.bracketMatchForeground" = base06;
    "editorOverviewRuler.inlineChatInserted" = null;
    "editorOverviewRuler.inlineChatRemoved" = null;
    "editorError.foreground" = base08;
    "editorError.border" = null;
    "editorError.background" = null;
    "editorWarning.foreground" = base0A;
    "editorWarning.border" = null;
    "editorWarning.background" = null;
    "editorInfo.foreground" = base0C;
    "editorInfo.border" = null;
    "editorInfo.background" = null;
    "editorHint.foreground" = base0D;
    "editorHint.border" = null;
    "problemsErrorIcon.foreground" = base08;
    "problemsWarningIcon.foreground" = base0A;
    "problemsInfoIcon.foreground" = base0C;
    "editorUnnecessaryCode.border" = null;
    "editorUnnecessaryCode.opacity" = null;
    "editorGutter.background" = base00;
    "editorGutter.modifiedBackground" = base0E;
    "editorGutter.addedBackground" = base0B;
    "editorGutter.deletedBackground" = base08;
    "editorGutter.commentRangeForeground" = base04;
    "editorGutter.commentGlyphForeground" = null;
    "editorGutter.commentUnresolvedGlyphForeground" = null;
    "editorGutter.foldingControlForeground" = base05;
    "editorCommentsWidget.resolvedBorder" = null;
    "editorCommentsWidget.unresolvedBorder" = null;
    "editorCommentsWidget.rangeBackground" = null;
    "editorCommentsWidget.rangeActiveBackground" = null;
    "editorCommentsWidget.replyInputBackground" = null;
    "diffEditor.insertedTextBackground" = "${base0B}4c";
    "diffEditor.insertedTextBorder" = null;
    "diffEditor.removedTextBackground" = "${base08}4c";
    "diffEditor.removedTextBorder" = null;
    "diffEditor.border" = base02;
    "diffEditor.diagonalFill" = base02;
    "diffEditor.insertedLineBackground" = "${base0B}18";
    "diffEditor.removedLineBackground" = "${base08}18";
    "diffEditorGutter.insertedLineBackground" = "${base0B}99";
    "diffEditorGutter.removedLineBackground" = "${base08}99";
    "diffEditorOverview.insertedForeground" = "${base0B}99";
    "diffEditorOverview.removedForeground" = "${base08}99";
    "diffEditor.unchangedRegionBackground" = null;
    "diffEditor.unchangedRegionForeground" = null;
    "diffEditor.unchangedRegionShadow" = "#00000000";
    "diffEditor.unchangedCodeBackground" = null;
    "diffEditor.move.border" = null;
    "diffEditor.moveActive.border" = null;
    "multiDiffEditor.headerBackground" = null;
    "multiDiffEditor.background" = null;
    "multiDiffEditor.border" = null;
    "chat.requestBorder" = base02;
    "chat.requestBackground" = base01;
    "chat.slashCommandBackground" = base0D;
    "chat.slashCommandForeground" = base00;
    "chat.avatarBackground" = base0D;
    "chat.avatarForeground" = base00;
    "inlineChat.background" = base01;
    "inlineChat.border" = base02;
    "inlineChat.shadow" = "#00000000";
    "inlineChat.regionHighlight" = base01;
    "inlineChatInput.border" = base02;
    "inlineChatInput.focusBorder" = base0D;
    "inlineChatInput.placeholderForeground" = base03;
    "inlineChatInput.background" = base00;
    "inlineChatDiff.inserted" = "${base0B}60";
    "inlineChatDiff.removed" = "${base08}60";
    "interactive.activeCodeBorder" = null;
    "interactive.inactiveCodeBorder" = null;
    "editorWidget.foreground" = base05;
    "editorWidget.background" = base00;
    "editorWidget.border" = base02;
    "editorWidget.resizeBorder" = base0D;
    "editorSuggestWidget.background" = base01;
    "editorSuggestWidget.border" = base02;
    "editorSuggestWidget.foreground" = base05;
    "editorSuggestWidget.focusHighlightForeground" = base0D;
    "editorSuggestWidget.highlightForeground" = base0D;
    "editorSuggestWidget.selectedBackground" = base02;
    "editorSuggestWidget.selectedForeground" = base05;
    "editorSuggestWidget.selectedIconForeground" = base05;
    "editorSuggestWidgetStatus.foreground" = null;
    "editorHoverWidget.foreground" = base05;
    "editorHoverWidget.background" = base01;
    "editorHoverWidget.border" = base02;
    "editorHoverWidget.highlightForeground" = base0D;
    "editorHoverWidget.statusBarBackground" = base01;
    "editorGhostText.border" = null;
    "editorGhostText.background" = "#00000000";
    "editorGhostText.foreground" = base03;
    "editorStickyScroll.background" = base00;
    "editorStickyScroll.border" = base02;
    "editorStickyScroll.shadow" = "#00000000";
    "editorStickyScrollHover.background" = base01;
    "debugExceptionWidget.background" = base01;
    "debugExceptionWidget.border" = null;
    "editorMarkerNavigation.background" = base01;
    "editorMarkerNavigationError.background" = base08;
    "editorMarkerNavigationWarning.background" = base0A;
    "editorMarkerNavigationInfo.background" = base0D;
    "editorMarkerNavigationError.headerBackground" = "${base08}20";
    "editorMarkerNavigationWarning.headerBackground" = "${base0A}20";
    "editorMarkerNavigationInfo.headerBackground" = "${base0C}20";
    "peekView.border" = null;
    "peekViewEditor.background" = base01;
    "peekViewEditorGutter.background" = base01;
    "peekViewEditor.matchHighlightBackground" = base09;
    "peekViewEditor.matchHighlightBorder" = null;
    "peekViewResult.background" = base00;
    "peekViewResult.fileForeground" = base05;
    "peekViewResult.lineForeground" = base03;
    "peekViewResult.matchHighlightBackground" = base09;
    "peekViewResult.selectionBackground" = base02;
    "peekViewResult.selectionForeground" = base05;
    "peekViewTitle.background" = base02;
    "peekViewTitleDescription.foreground" = base03;
    "peekViewTitleLabel.foreground" = base05;
    "peekViewEditorStickyScroll.background" = null;
    "merge.currentHeaderBackground" = "${base0D}66";
    "merge.currentContentBackground" = "${base0D}18";
    "merge.incomingHeaderBackground" = "${base0B}66";
    "merge.incomingContentBackground" = "${base0B}18";
    "merge.border" = null;
    "merge.commonContentBackground" = null;
    "merge.commonHeaderBackground" = null;
    "editorOverviewRuler.currentContentForeground" = base0D;
    "editorOverviewRuler.incomingContentForeground" = base0B;
    "editorOverviewRuler.commonContentForeground" = base0F;
    "editorOverviewRuler.commentForeground" = null;
    "editorOverviewRuler.commentUnresolvedForeground" = null;
    "mergeEditor.change.background" = null;
    "mergeEditor.change.word.background" = null;
    "mergeEditor.conflict.unhandledUnfocused.border" = null;
    "mergeEditor.conflict.unhandledFocused.border" = base0D;
    "mergeEditor.conflict.handledUnfocused.border" = null;
    "mergeEditor.conflict.handledFocused.border" = base0D;
    "mergeEditor.conflict.handled.minimapOverViewRuler" = null;
    "mergeEditor.conflict.unhandled.minimapOverViewRuler" = null;
    "mergeEditor.conflictingLines.background" = null;
    "mergeEditor.changeBase.background" = null;
    "mergeEditor.changeBase.word.background" = null;
    "mergeEditor.conflict.input1.background" = null;
    "mergeEditor.conflict.input2.background" = null;
    "panel.background" = base01;
    "panel.border" = "#00000000";
    "panel.dropBorder" = base01;
    "panelTitle.activeBorder" = null;
    "panelTitle.activeForeground" = base05;
    "panelTitle.inactiveForeground" = base03;
    "panelInput.border" = null;
    "panelSection.border" = null;
    "panelSection.dropBackground" = null;
    "panelSectionHeader.background" = null;
    "panelSectionHeader.foreground" = null;
    "panelSectionHeader.border" = null;
    "outputView.background" = null;
    "outputViewStickyScroll.background" = null;
    "statusBar.background" = base01;
    "statusBar.foreground" = base05;
    "statusBar.border" = null;
    "statusBar.debuggingBackground" = base09;
    "statusBar.debuggingForeground" = base00;
    "statusBar.debuggingBorder" = null;
    "statusBar.noFolderForeground" = base05;
    "statusBar.noFolderBackground" = base01;
    "statusBar.noFolderBorder" = null;
    "statusBarItem.activeBackground" = base02;
    "statusBarItem.hoverForeground" = base05;
    "statusBarItem.hoverBackground" = base02;
    "statusBarItem.prominentForeground" = base00;
    "statusBarItem.prominentBackground" = base0E;
    "statusBarItem.prominentHoverForeground" = base00;
    "statusBarItem.prominentHoverBackground" = "${base0E}C0";
    "statusBarItem.remoteBackground" = base01;
    "statusBarItem.remoteForeground" = base05;
    "statusBarItem.remoteHoverBackground" = base02;
    "statusBarItem.remoteHoverForeground" = base05;
    "statusBarItem.errorBackground" = base08;
    "statusBarItem.errorForeground" = base00;
    "statusBarItem.errorHoverBackground" = "${base08}C0";
    "statusBarItem.errorHoverForeground" = base00;
    "statusBarItem.warningBackground" = base0A;
    "statusBarItem.warningForeground" = base00;
    "statusBarItem.warningHoverBackground" = "${base0A}C0";
    "statusBarItem.warningHoverForeground" = base00;
    "statusBarItem.compactHoverBackground" = base02;
    "statusBarItem.focusBorder" = base0D;
    "statusBar.focusBorder" = base0D;
    "statusBarItem.offlineBackground" = base09;
    "statusBarItem.offlineForeground" = base00;
    "statusBarItem.offlineHoverForeground" = base00;
    "statusBarItem.offlineHoverBackground" = "${base09}C0";
    "titleBar.activeBackground" = base01;
    "titleBar.activeForeground" = base05;
    "titleBar.inactiveBackground" = base01;
    "titleBar.inactiveForeground" = base03;
    "titleBar.border" = null;
    "menubar.selectionForeground" = base05;
    "menubar.selectionBackground" = base02;
    "menubar.selectionBorder" = null;
    "menu.foreground" = base05;
    "menu.background" = base01;
    "menu.selectionForeground" = base05;
    "menu.selectionBackground" = base02;
    "menu.selectionBorder" = null;
    "menu.separatorBackground" = base02;
    "menu.border" = base02;
    "commandCenter.foreground" = base05;
    "commandCenter.activeForeground" = base05;
    "commandCenter.background" = base00;
    "commandCenter.activeBackground" = base00;
    "commandCenter.border" = null;
    "commandCenter.inactiveForeground" = null;
    "commandCenter.inactiveBorder" = null;
    "commandCenter.activeBorder" = null;
    "commandCenter.debuggingBackground" = null;
    "notificationCenter.border" = null;
    "notificationCenterHeader.foreground" = base05;
    "notificationCenterHeader.background" = base01;
    "notificationToast.border" = null;
    "notifications.foreground" = base05;
    "notifications.background" = base02;
    "notifications.border" = null;
    "notificationLink.foreground" = base0D;
    "notificationsErrorIcon.foreground" = base08;
    "notificationsWarningIcon.foreground" = base0A;
    "notificationsInfoIcon.foreground" = base0D;
    "banner.background" = base02;
    "banner.foreground" = base05;
    "banner.iconForeground" = base0D;
    "extensionButton.prominentForeground" = base00;
    "extensionButton.prominentBackground" = base0B;
    "extensionButton.prominentHoverBackground" = "${base0B}C0";
    "extensionButton.background" = base0D;
    "extensionButton.foreground" = base00;
    "extensionButton.hoverBackground" = "${base0D}C0";
    "extensionButton.separator" = "#00000000";
    "extensionBadge.remoteBackground" = base09;
    "extensionBadge.remoteForeground" = base07;
    "extensionIcon.starForeground" = base0A;
    "extensionIcon.verifiedForeground" = base0D;
    "extensionIcon.preReleaseForeground" = base09;
    "extensionIcon.sponsorForeground" = null;
    "pickerGroup.border" = base02;
    "pickerGroup.foreground" = base03;
    "quickInput.background" = base01;
    "quickInput.foreground" = base05;
    "quickInputList.focusBackground" = base02;
    "quickInputList.focusForeground" = base05;
    "quickInputList.focusIconForeground" = base05;
    "quickInputTitle.background" = base01;
    "keybindingLabel.background" = base02;
    "keybindingLabel.foreground" = base05;
    "keybindingLabel.border" = null;
    "keybindingLabel.bottomBorder" = base02;
    "keybindingTable.headerBackground" = base02;
    "keybindingTable.rowsBackground" = base01;
    "terminal.background" = base00;
    "terminal.border" = null;
    "terminal.foreground" = base05;
    "terminal.ansiBlack" = base00;
    "terminal.ansiBlue" = base0D;
    "terminal.ansiBrightBlack" = base03;
    "terminal.ansiBrightBlue" = base0D;
    "terminal.ansiBrightCyan" = base0C;
    "terminal.ansiBrightGreen" = base0B;
    "terminal.ansiBrightMagenta" = base0E;
    "terminal.ansiBrightRed" = base08;
    "terminal.ansiBrightWhite" = base07;
    "terminal.ansiBrightYellow" = base0A;
    "terminal.ansiCyan" = base0C;
    "terminal.ansiGreen" = base0B;
    "terminal.ansiMagenta" = base0E;
    "terminal.ansiRed" = base08;
    "terminal.ansiWhite" = base05;
    "terminal.ansiYellow" = base0A;
    "terminal.selectionBackground" = null;
    "terminal.selectionForeground" = null;
    "terminal.inactiveSelectionBackground" = null;
    "terminal.findMatchBackground" = null;
    "terminal.findMatchBorder" = null;
    "terminal.findMatchHighlightBackground" = null;
    "terminal.findMatchHighlightBorder" = null;
    "terminal.hoverHighlightBackground" = null;
    "terminalCursor.background" = null;
    "terminalCursor.foreground" = base05;
    "terminal.dropBackground" = null;
    "terminal.tab.activeBorder" = null;
    "terminalCommandDecoration.defaultBackground" = null;
    "terminalCommandDecoration.successBackground" = null;
    "terminalCommandDecoration.errorBackground" = null;
    "terminalOverviewRuler.cursorForeground" = "#ff0000";
    "terminalOverviewRuler.findMatchForeground" = "#ff0000";
    "terminalStickyScroll.background" = null;
    "terminalStickyScrollHover.background" = null;
    "debugToolBar.background" = base01;
    "debugToolBar.border" = null;
    "editor.stackFrameHighlightBackground" = null;
    "editor.focusedStackFrameHighlightBackground" = null;
    "editor.inlineValuesForeground" = null;
    "editor.inlineValuesBackground" = null;
    "debugView.exceptionLabelForeground" = null;
    "debugView.exceptionLabelBackground" = null;
    "debugView.stateLabelForeground" = base07;
    "debugView.stateLabelBackground" = base0D;
    "debugView.valueChangedHighlight" = base0D;
    "debugTokenExpression.name" = base0E;
    "debugTokenExpression.value" = base05;
    "debugTokenExpression.string" = base0B;
    "debugTokenExpression.boolean" = base09;
    "debugTokenExpression.number" = base09;
    "debugTokenExpression.error" = base08;
    "testing.iconFailed" = base08;
    "testing.iconErrored" = base0F;
    "testing.iconPassed" = base0B;
    "testing.runAction" = base04;
    "testing.iconQueued" = base0A;
    "testing.iconUnset" = base04;
    "testing.iconSkipped" = base0E;
    "testing.peekBorder" = null;
    "testing.peekHeaderBackground" = base01;
    "testing.message.error.decorationForeground" = base05;
    "testing.message.error.lineBackground" = "${base08}20";
    "testing.message.info.decorationForeground" = base05;
    "testing.message.info.lineBackground" = "${base0D}20";
    "testing.messagePeekBorder" = null;
    "testing.messagePeekHeaderBackground" = null;
    "testing.coveredBackground" = null;
    "testing.coveredBorder" = null;
    "testing.coveredGutterBackground" = null;
    "testing.uncoveredBranchBackground" = null;
    "testing.uncoveredBackground" = null;
    "testing.uncoveredBorder" = null;
    "testing.uncoveredGutterBackground" = null;
    "testing.coverCountBadgeBackground" = null;
    "testing.coverCountBadgeForeground" = null;
    "welcomePage.background" = base00;
    "welcomePage.progress.background" = base03;
    "welcomePage.progress.foreground" = base0D;
    "welcomePage.tileBackground" = base01;
    "welcomePage.tileHoverBackground" = base02;
    "welcomePage.tileBorder" = null;
    "walkThrough.embeddedEditorBackground" = base00;
    "walkthrough.stepTitle.foreground" = null;
    "gitDecoration.addedResourceForeground" = base0B;
    "gitDecoration.modifiedResourceForeground" = base0E;
    "gitDecoration.deletedResourceForeground" = base08;
    "gitDecoration.renamedResourceForeground" = base0C;
    "gitDecoration.stageModifiedResourceForeground" = base0E;
    "gitDecoration.stageDeletedResourceForeground" = base08;
    "gitDecoration.untrackedResourceForeground" = base09;
    "gitDecoration.ignoredResourceForeground" = base03;
    "gitDecoration.conflictingResourceForeground" = base0A;
    "gitDecoration.submoduleResourceForeground" = base0F;
    "settings.headerForeground" = base05;
    "settings.modifiedItemIndicator" = base0D;
    "settings.dropdownBackground" = base01;
    "settings.dropdownForeground" = base05;
    "settings.dropdownBorder" = null;
    "settings.dropdownListBorder" = null;
    "settings.checkboxBackground" = base01;
    "settings.checkboxForeground" = base05;
    "settings.checkboxBorder" = null;
    "settings.rowHoverBackground" = base02;
    "settings.textInputBackground" = base01;
    "settings.textInputForeground" = base05;
    "settings.textInputBorder" = null;
    "settings.numberInputBackground" = base01;
    "settings.numberInputForeground" = base05;
    "settings.numberInputBorder" = null;
    "settings.focusedRowBackground" = base02;
    "settings.focusedRowBorder" = base0D;
    "settings.headerBorder" = base05;
    "settings.sashBorder" = base05;
    "settings.settingsHeaderHoverForeground" = null;
    "breadcrumb.foreground" = base05;
    "breadcrumb.background" = base01;
    "breadcrumb.focusForeground" = base06;
    "breadcrumb.activeSelectionForeground" = base07;
    "breadcrumbPicker.background" = base01;
    "editor.snippetTabstopHighlightBackground" = base02;
    "editor.snippetTabstopHighlightBorder" = null;
    "editor.snippetFinalTabstopHighlightBackground" = base03;
    "editor.snippetFinalTabstopHighlightBorder" = null;
    "symbolIcon.arrayForeground" = base05;
    "symbolIcon.booleanForeground" = base09;
    "symbolIcon.classForeground" = base0A;
    "symbolIcon.colorForeground" = base05;
    "symbolIcon.constantForeground" = base09;
    "symbolIcon.constructorForeground" = base0D;
    "symbolIcon.enumeratorForeground" = base09;
    "symbolIcon.enumeratorMemberForeground" = base0D;
    "symbolIcon.eventForeground" = base0A;
    "symbolIcon.fieldForeground" = base08;
    "symbolIcon.fileForeground" = base05;
    "symbolIcon.folderForeground" = base05;
    "symbolIcon.functionForeground" = base0D;
    "symbolIcon.interfaceForeground" = base0D;
    "symbolIcon.keyForeground" = base05;
    "symbolIcon.keywordForeground" = base0E;
    "symbolIcon.methodForeground" = base0D;
    "symbolIcon.moduleForeground" = base05;
    "symbolIcon.namespaceForeground" = base05;
    "symbolIcon.nullForeground" = base0F;
    "symbolIcon.numberForeground" = base09;
    "symbolIcon.objectForeground" = base05;
    "symbolIcon.operatorForeground" = base05;
    "symbolIcon.packageForeground" = base05;
    "symbolIcon.propertyForeground" = base05;
    "symbolIcon.referenceForeground" = base05;
    "symbolIcon.snippetForeground" = base05;
    "symbolIcon.stringForeground" = base0B;
    "symbolIcon.structForeground" = base0A;
    "symbolIcon.textForeground" = base05;
    "symbolIcon.typeParameterForeground" = base05;
    "symbolIcon.unitForeground" = base05;
    "symbolIcon.variableForeground" = base08;
    "debugIcon.breakpointForeground" = base08;
    "debugIcon.breakpointDisabledForeground" = base04;
    "debugIcon.breakpointUnverifiedForeground" = base02;
    "debugIcon.breakpointCurrentStackframeForeground" = base0A;
    "debugIcon.breakpointStackframeForeground" = base0F;
    "debugIcon.startForeground" = base0B;
    "debugIcon.pauseForeground" = base0D;
    "debugIcon.stopForeground" = base08;
    "debugIcon.disconnectForeground" = base08;
    "debugIcon.restartForeground" = base0B;
    "debugIcon.stepOverForeground" = base0D;
    "debugIcon.stepIntoForeground" = base0C;
    "debugIcon.stepOutForeground" = base0E;
    "debugIcon.continueForeground" = base0B;
    "debugIcon.stepBackForeground" = base0F;
    "debugConsole.infoForeground" = base05;
    "debugConsole.warningForeground" = base0A;
    "debugConsole.errorForeground" = base08;
    "debugConsole.sourceForeground" = base05;
    "debugConsoleInputIcon.foreground" = base05;
    "notebook.editorBackground" = base00;
    "notebook.cellBorderColor" = base03;
    "notebook.cellHoverBackground" = base01;
    "notebook.cellInsertionIndicator" = null;
    "notebook.cellStatusBarItemHoverBackground" = null;
    "notebook.cellToolbarSeparator" = base02;
    "notebook.cellEditorBackground" = base00;
    "notebook.focusedCellBackground" = base02;
    "notebook.focusedCellBorder" = base0D;
    "notebook.focusedEditorBorder" = base0D;
    "notebook.inactiveFocusedCellBorder" = base03;
    "notebook.inactiveSelectedCellBorder" = null;
    "notebook.outputContainerBackgroundColor" = null;
    "notebook.outputContainerBorderColor" = null;
    "notebook.selectedCellBackground" = base02;
    "notebook.selectedCellBorder" = null;
    "notebook.symbolHighlightBackground" = null;
    "notebookScrollbarSlider.activeBackground" = null;
    "notebookScrollbarSlider.background" = null;
    "notebookScrollbarSlider.hoverBackground" = null;
    "notebookStatusErrorIcon.foreground" = base08;
    "notebookStatusRunningIcon.foreground" = base0C;
    "notebookStatusSuccessIcon.foreground" = base0B;
    "notebookEditorOverviewRuler.runningCellForeground" = null;
    "charts.foreground" = base05;
    "charts.lines" = base05;
    "charts.red" = base08;
    "charts.blue" = base0D;
    "charts.yellow" = base0A;
    "charts.orange" = base09;
    "charts.green" = base0B;
    "charts.purple" = base0E;
    "ports.iconRunningProcessForeground" = base09;
    "commentsView.resolvedIcon" = null;
    "commentsView.unresolvedIcon" = null;
    "actionBar.toggledBackground" = null;
    "simpleFindWidget.sashBorder" = null;
    "scm.historyItemAdditionsForeground" = null;
    "scm.historyItemDeletionsForeground" = null;
    "scm.historyItemStatisticsBorder" = null;
    "scm.historyItemSelectedStatisticsBorder" = null;
  };
  tokenColors = [
    {
      name = "Comment";
      scope = [
        "comment"
        "punctuation.definition.comment"
      ];
      settings = {
        fontStyle = "italic";
        foreground = base03;
      };
    }
    {
      name = "Variables, Parameters";
      scope = [
        "variable"
        "string constant.other.placeholder"
        "entity.name.variable.parameter"
        "entity.name.variable.local"
        "variable.parameter"
      ];
      settings.foreground = base08;
    }
    {
      name = "Properties";
      scope = [ "variable.other.object.property" ];
      settings.foreground = base0D;
    }
    {
      name = "Colors";
      scope = [ "constant.other.color" ];
      settings.foreground = base0B;
    }
    {
      name = "Invalid";
      scope = [
        "invalid"
        "invalid.illegal"
      ];
      settings.foreground = base08;
    }
    {
      name = "Invalid - Deprecated";
      scope = [ "invalid.deprecated" ];
      settings.foreground = base0F;
    }
    {
      name = "Keyword, Storage";
      scope = [
        "keyword"
        "keyword.other"
        "keyword.other.using"
        "keyword.other.namespace"
        "keyword.other.class"
        "keyword.other.new"
        "keyword.other.event"
        "keyword.other.this"
        "keyword.other.await"
        "keyword.other.var"
        "keyword.other.package"
        "keyword.other.import"
        "variable.language.this"
        "storage.type.ts"
        "storage.modifier"
      ];
      settings.foreground = base0E;
    }
    {
      name = "Keyword Control";
      scope = [
        "keyword.control"
        "keyword.control.flow"
        "keyword.control.from"
        "keyword.control.import"
        "keyword.control.as"
      ];
      settings.foreground = base0E;
    }
    {
      name = "Types, Primitives";
      scope = [
        "keyword.type"
        "storage.type.primitive"
      ];
      settings.foreground = base0C;
    }
    {
      name = "Function";
      scope = [ "storage.type.function" ];
      settings.foreground = base0D;
    }
    {
      name = "Operator, Misc";
      scope = [
        "constant.other.color"
        "punctuation"
        "punctuation.section.class.end"
        "meta.tag"
        "punctuation.definition.tag"
        "punctuation.separator.inheritance.php"
        "punctuation.definition.tag.html"
        "punctuation.definition.tag.begin.html"
        "punctuation.definition.tag.end.html"
        "keyword.other.template"
        "keyword.other.substitution"
      ];
      settings.foreground = base05;
    }
    {
      name = "Embedded";
      scope = [
        "punctuation.section.embedded"
        "variable.interpolation"
      ];
      settings.foreground = base0F;
    }
    {
      name = "Tag";
      scope = [
        "entity.name.tag"
        "meta.tag.sgml"
        "markup.deleted.git_gutter"
      ];
      settings.foreground = base08;
    }
    {
      name = "Function, Special Method";
      scope = [
        "entity.name.function"
        "meta.function-call"
        "variable.function"
        "support.function"
        "keyword.other.special-method"
      ];
      settings.foreground = base0D;
    }
    {
      name = "Block Level Variables";
      scope = [
        "meta.block variable.other"
      ];
      settings.foreground = base08;
    }
    {
      name = "Other Variable, String Link";
      scope = [
        "support.other.variable"
        "string.other.link"
      ];
      settings.foreground = base08;
    }
    {
      name = "Number, Constant, Function Argument, Tag Attribute, Embedded";
      scope = [
        "constant.numeric"
        "constant.language"
        "support.constant"
        "constant.character"
        "constant.escape"
        "keyword.other.unit"
      ];
      settings.foreground = base09;
    }
    {
      name = "String, Symbols, Inherited Class, Markup Heading";
      scope = [
        "string"
        "constant.other.symbol"
        "constant.other.key"
        "entity.other.inherited-class"
        "markup.heading"
        "markup.inserted.git_gutter"
        "meta.group.braces.curly constant.other.object.key.js string.unquoted.label.js"
      ];
      settings = {
        fontStyle = "";
        foreground = base0B;
      };
    }
    {
      name = "Class, Support";
      scope = [
        "entity.name"
        "support.type"
        "support.class"
        "support.other.namespace.use.php"
        "meta.use.php"
        "support.other.namespace.php"
        "markup.changed.git_gutter"
        "support.type.sys-types"
      ];
      settings.foreground = base0A;
    }
    {
      name = "Storage Type, Import Class";
      scope = [
        "storage.type"
        "storage.modifier.package"
        "storage.modifier.import"
      ];
      settings.foreground = base0A;
    }
    {
      name = "Fields";
      scope = [ "entity.name.variable.field" ];
      settings.foreground = base0D;
    }
    {
      name = "Entity Types";
      scope = [ "support.type" ];
      settings.foreground = base0C;
    }
    {
      name = "CSS Class and Support";
      scope = [
        "source.css support.type.property-name"
        "source.sass support.type.property-name"
        "source.scss support.type.property-name"
        "source.less support.type.property-name"
        "source.stylus support.type.property-name"
        "source.postcss support.type.property-name"
      ];
      settings.foreground = base0C;
    }
    {
      name = "Sub-methods";
      scope = [
        "entity.name.module.js"
        "variable.import.parameter.js"
        "variable.other.class.js"
      ];
      settings.foreground = base08;
    }
    {
      name = "Language methods";
      scope = [
        "variable.language"
      ];
      settings = {
        fontStyle = "italic";
        foreground = base08;
      };
    }
    {
      name = "entity.name.method.js";
      scope = [
        "entity.name.method.js"
      ];
      settings = {
        fontStyle = "italic";
        foreground = base0D;
      };
    }
    {
      name = "meta.method.js";
      scope = [
        "meta.class-method.js entity.name.function.js"
        "variable.function.constructor"
      ];
      settings.foreground = base0D;
    }
    {
      name = "Attributes";
      scope = [ "entity.other.attribute-name" ];
      settings.foreground = base0D;
    }
    {
      name = "HTML Attributes";
      scope = [
        "text.html.basic entity.other.attribute-name.html"
        "text.html.basic entity.other.attribute-name"
      ];
      settings = {
        fontStyle = "italic";
        foreground = base0A;
      };
    }
    {
      name = "CSS Classes";
      scope = [ "entity.other.attribute-name.class" ];
      settings.foreground = base0A;
    }
    {
      name = "CSS ID's";
      scope = [ "source.sass keyword.control" ];
      settings.foreground = base0D;
    }
    {
      name = "Inserted";
      scope = [ "markup.inserted" ];
      settings.foreground = base0B;
    }
    {
      name = "Deleted";
      scope = [ "markup.deleted" ];
      settings.foreground = base08;
    }
    {
      name = "Changed";
      scope = [ "markup.changed" ];
      settings.foreground = base0E;
    }
    {
      name = "Regular Expressions";
      scope = [ "string.regexp" ];
      settings.foreground = base0C;
    }
    {
      name = "Escape Characters";
      scope = [ "constant.character.escape" ];
      settings.foreground = base0C;
    }
    {
      name = "URL";
      scope = [
        "*url*"
        "*link*"
        "*uri*"
      ];
      settings.fontStyle = "underline";
    }
    {
      name = "Decorators";
      scope = [
        "tag.decorator.js entity.name.tag.js"
        "tag.decorator.js punctuation.definition.tag.js"
      ];
      settings = {
        fontStyle = "italic";
        foreground = base0D;
      };
    }
    {
      name = "ES7 Bind Operator";
      scope = [
        "source.js constant.other.object.key.js string.unquoted.label.js"
      ];
      settings = {
        fontStyle = "italic";
        foreground = base0E;
      };
    }
    {
      name = "JSON Key - Level 0";
      scope = [
        "source.json meta.structure.dictionary.json support.type.property-name.json"
      ];
      settings.foreground = base0D;
    }
    {
      name = "JSON Key - Level 1";
      scope = [
        "source.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json support.type.property-name.json"
      ];
      settings.foreground = base0D;
    }
    {
      name = "JSON Key - Level 2";
      scope = [
        "source.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json support.type.property-name.json"
      ];
      settings.foreground = base0D;
    }
    {
      name = "JSON Key - Level 3";
      scope = [
        "source.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json support.type.property-name.json"
      ];
      settings.foreground = base0D;
    }
    {
      name = "JSON Key - Level 4";
      scope = [
        "source.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json support.type.property-name.json"
      ];
      settings.foreground = base0D;
    }
    {
      name = "JSON Key - Level 5";
      scope = [
        "source.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json support.type.property-name.json"
      ];
      settings.foreground = base0D;
    }
    {
      name = "JSON Key - Level 6";
      scope = [
        "source.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json support.type.property-name.json"
      ];
      settings.foreground = base0D;
    }
    {
      name = "JSON Key - Level 7";
      scope = [
        "source.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json support.type.property-name.json"
      ];
      settings.foreground = base0D;
    }
    {
      name = "JSON Key - Level 8";
      scope = [
        "source.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json support.type.property-name.json"
      ];
      settings.foreground = base0D;
    }
    {
      name = "Markdown - Plain";
      scope = [
        "text.html.markdown"
        "punctuation.definition.list_item.markdown"
      ];
      settings.foreground = base05;
    }
    {
      name = "Markdown - Markup Raw Inline";
      scope = [ "text.html.markdown markup.inline.raw.markdown" ];
      settings.foreground = base0E;
    }
    {
      name = "Markdown - Markup Raw Inline Punctuation";
      scope = [
        "text.html.markdown markup.inline.raw.markdown punctuation.definition.raw.markdown"
      ];
      settings.foreground = base0C;
    }
    {
      name = "Markdown - Line Break";
      scope = [ "text.html.markdown meta.dummy.line-break" ];
      settings.foreground = base03;
    }
    {
      name = "Markdown - Heading";
      scope = [
        "markdown.heading"
        "markup.heading | markup.heading entity.name"
        "markup.heading.markdown punctuation.definition.heading.markdown"
      ];
      settings.foreground = base0D;
    }
    {
      name = "Markup - Italic";
      scope = [
        "markup.italic"
      ];
      settings = {
        fontStyle = "italic";
        foreground = base08;
      };
    }
    {
      name = "Markup - Bold";
      scope = [
        "markup.bold"
        "markup.bold string"
      ];
      settings = {
        fontStyle = "bold";
        foreground = base08;
      };
    }
    {
      name = "Markup - Bold-Italic";
      scope = [
        "markup.bold markup.italic"
        "markup.italic markup.bold"
        "markup.quote markup.bold"
        "markup.bold markup.italic string"
        "markup.italic markup.bold string"
        "markup.quote markup.bold string"
      ];
      settings = {
        fontStyle = "bold";
        foreground = base08;
      };
    }
    {
      name = "Markup - Underline";
      scope = [ "markup.underline" ];
      settings = {
        fontStyle = "underline";
        foreground = base09;
      };
    }
    {
      name = "Markdown - Blockquote";
      scope = [ "markup.quote punctuation.definition.blockquote.markdown" ];
      settings.foreground = base0C;
    }
    {
      name = "Markup - Quote";
      scope = [ "markup.quote" ];
      settings.fontStyle = "italic";
    }
    {
      name = "Markdown - Link";
      scope = [ "string.other.link.title.markdown" ];
      settings.foreground = base0D;
    }
    {
      name = "Markdown - Link Description";
      scope = [ "string.other.link.description.title.markdown" ];
      settings.foreground = base0E;
    }
    {
      name = "Markdown - Link Anchor";
      scope = [ "constant.other.reference.link.markdown" ];
      settings.foreground = base0A;
    }
    {
      name = "Markup - Raw Block";
      scope = [ "markup.raw.block" ];
      settings.foreground = base0E;
    }
    {
      name = "Markdown - Raw Block Fenced";
      scope = [ "markup.raw.block.fenced.markdown" ];
      settings.foreground = "#00000050";
    }
    {
      name = "Markdown - Fenced Bode Block";
      scope = [
        "punctuation.definition.fenced.markdown"
      ];
      settings.foreground = "#00000050";
    }
    {
      name = "Markdown - Fenced Code Block Variable";
      scope = [
        "markup.raw.block.fenced.markdown"
        "variable.language.fenced.markdown"
      ];
      settings.foreground = base0E;
    }
    {
      name = "Markdown - Fenced Language";
      scope = [ "variable.language.fenced.markdown" ];
      settings.foreground = base08;
    }
    {
      name = "Markdown - Separator";
      scope = [ "meta.separator" ];
      settings = {
        fontStyle = "bold";
        foreground = base0C;
      };
    }
    {
      name = "Markup - Table";
      scope = [ "markup.table" ];
      settings.foreground = base0E;
    }
    {
      scope = "token.info-token";
      settings.foreground = base0D;
    }
    {
      scope = "token.warn-token";
      settings.foreground = base0A;
    }
    {
      scope = "token.error-token";
      settings.foreground = base08;
    }
    {
      scope = "token.debug-token";
      settings.foreground = base0E;
    }
  ];
}
