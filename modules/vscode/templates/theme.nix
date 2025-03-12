{ config, ... }:

with config.lib.stylix.colors.withHashtag;

{
  "$schema" = "vscode://schemas/color-theme";
  name = scheme;
  type = "dark";
  colors = {
    "actionBar.toggledBackground" = null;
    "activityBar.activeBackground" = base02;
    "activityBar.activeBorder" = base05;
    "activityBar.activeFocusBorder" = base0D;
    "activityBar.background" = base01;
    "activityBar.border" = null;
    "activityBar.dropBorder" = base03;
    "activityBar.foreground" = base05;
    "activityBar.inactiveForeground" = base03;
    "activityBarBadge.background" = base0D;
    "activityBarBadge.foreground" = base00;
    "activityBarTop.activeBackground" = null;
    "activityBarTop.activeBorder" = base0D;
    "activityBarTop.background" = null;
    "activityBarTop.dropBorder" = base03;
    "activityBarTop.foreground" = base05;
    "activityBarTop.inactiveForeground" = base03;
    "activityErrorBadge.background" = null;
    "activityErrorBadge.foreground" = null;
    "activityWarningBadge.background" = null;
    "activityWarningBadge.foreground" = null;
    "badge.background" = base00;
    "badge.foreground" = base05;
    "banner.background" = base02;
    "banner.foreground" = base05;
    "banner.iconForeground" = base0D;
    "breadcrumb.activeSelectionForeground" = base07;
    "breadcrumb.background" = base01;
    "breadcrumb.focusForeground" = base06;
    "breadcrumb.foreground" = base05;
    "breadcrumbPicker.background" = base01;
    "button.background" = base0D;
    "button.border" = null;
    "button.foreground" = base00;
    "button.hoverBackground" = "${base0D}C0";
    "button.secondaryBackground" = base0E;
    "button.secondaryForeground" = base00;
    "button.secondaryHoverBackground" = "${base0E}C0";
    "button.separator" = null;
    "chart.axis" = null;
    "chart.guide" = null;
    "chart.line" = null;
    "charts.blue" = base0D;
    "charts.foreground" = base05;
    "charts.green" = base0B;
    "charts.lines" = base05;
    "charts.orange" = base09;
    "charts.purple" = base0E;
    "charts.red" = base08;
    "charts.yellow" = base0A;
    "chat.avatarBackground" = base0D;
    "chat.avatarForeground" = base00;
    "chat.editedFileForeground" = null;
    "chat.requestBackground" = base01;
    "chat.requestBorder" = base02;
    "chat.slashCommandBackground" = base0D;
    "chat.slashCommandForeground" = base00;
    "checkbox.background" = base00;
    "checkbox.border" = null;
    "checkbox.foreground" = base05;
    "checkbox.selectBackground" = null;
    "checkbox.selectBorder" = null;
    "commandCenter.activeBackground" = base00;
    "commandCenter.activeBorder" = null;
    "commandCenter.activeForeground" = base05;
    "commandCenter.background" = base00;
    "commandCenter.border" = null;
    "commandCenter.debuggingBackground" = null;
    "commandCenter.foreground" = base05;
    "commandCenter.inactiveBorder" = null;
    "commandCenter.inactiveForeground" = null;
    "commentsView.resolvedIcon" = null;
    "commentsView.unresolvedIcon" = null;
    "contrastActiveBorder" = null;
    "contrastBorder" = null;
    "debugConsole.errorForeground" = base08;
    "debugConsole.infoForeground" = base05;
    "debugConsole.sourceForeground" = base05;
    "debugConsole.warningForeground" = base0A;
    "debugConsoleInputIcon.foreground" = base05;
    "debugExceptionWidget.background" = base01;
    "debugExceptionWidget.border" = null;
    "debugIcon.breakpointCurrentStackframeForeground" = base0A;
    "debugIcon.breakpointDisabledForeground" = base04;
    "debugIcon.breakpointForeground" = base08;
    "debugIcon.breakpointStackframeForeground" = base0F;
    "debugIcon.breakpointUnverifiedForeground" = base02;
    "debugIcon.continueForeground" = base0B;
    "debugIcon.disconnectForeground" = base08;
    "debugIcon.pauseForeground" = base0D;
    "debugIcon.restartForeground" = base0B;
    "debugIcon.startForeground" = base0B;
    "debugIcon.stepBackForeground" = base0F;
    "debugIcon.stepIntoForeground" = base0C;
    "debugIcon.stepOutForeground" = base0E;
    "debugIcon.stepOverForeground" = base0D;
    "debugIcon.stopForeground" = base08;
    "debugTokenExpression.boolean" = base09;
    "debugTokenExpression.error" = base08;
    "debugTokenExpression.name" = base0E;
    "debugTokenExpression.number" = base09;
    "debugTokenExpression.string" = base0B;
    "debugTokenExpression.type" = null;
    "debugTokenExpression.value" = base05;
    "debugToolBar.background" = base01;
    "debugToolBar.border" = null;
    "debugView.exceptionLabelBackground" = null;
    "debugView.exceptionLabelForeground" = null;
    "debugView.stateLabelBackground" = base0D;
    "debugView.stateLabelForeground" = base07;
    "debugView.valueChangedHighlight" = base0D;
    "descriptionForeground" = "${base05}99";
    "diffEditor.border" = base02;
    "diffEditor.diagonalFill" = base02;
    "diffEditor.insertedLineBackground" = "${base0B}18";
    "diffEditor.insertedTextBackground" = "${base0B}4c";
    "diffEditor.insertedTextBorder" = null;
    "diffEditor.move.border" = null;
    "diffEditor.moveActive.border" = null;
    "diffEditor.removedLineBackground" = "${base08}18";
    "diffEditor.removedTextBackground" = "${base08}4c";
    "diffEditor.removedTextBorder" = null;
    "diffEditor.unchangedCodeBackground" = null;
    "diffEditor.unchangedRegionBackground" = null;
    "diffEditor.unchangedRegionForeground" = null;
    "diffEditor.unchangedRegionShadow" = "#00000000";
    "diffEditorGutter.insertedLineBackground" = "${base0B}99";
    "diffEditorGutter.removedLineBackground" = "${base08}99";
    "diffEditorOverview.insertedForeground" = "${base0B}99";
    "diffEditorOverview.removedForeground" = "${base08}99";
    "disabledForeground" = base04;
    "dropdown.background" = base00;
    "dropdown.border" = null;
    "dropdown.foreground" = base05;
    "dropdown.listBackground" = base00;
    "editor.background" = base00;
    "editor.compositionBorder" = null;
    "editor.findMatchBackground" = "${base0A}18";
    "editor.findMatchBorder" = base0A;
    "editor.findMatchForeground" = null;
    "editor.findMatchHighlightBackground" = "${base0A}66";
    "editor.findMatchHighlightBorder" = null;
    "editor.findMatchHighlightForeground" = null;
    "editor.findRangeHighlightBackground" = base01;
    "editor.findRangeHighlightBorder" = null;
    "editor.focusedStackFrameHighlightBackground" = null;
    "editor.foldBackground" = null;
    "editor.foldPlaceholderForeground" = null;
    "editor.foreground" = base05;
    "editor.hoverHighlightBackground" = base02;
    "editor.inactiveSelectionBackground" = base02;
    "editor.inlineValuesBackground" = null;
    "editor.inlineValuesForeground" = null;
    "editor.lineHighlightBackground" = base01;
    "editor.lineHighlightBorder" = null;
    "editor.linkedEditingBackground" = null;
    "editor.placeholder.foreground" = null;
    "editor.rangeHighlightBackground" = base01;
    "editor.rangeHighlightBorder" = null;
    "editor.selectionBackground" = base02;
    "editor.selectionForeground" = null;
    "editor.selectionHighlightBackground" = base01;
    "editor.selectionHighlightBorder" = null;
    "editor.snippetFinalTabstopHighlightBackground" = base03;
    "editor.snippetFinalTabstopHighlightBorder" = null;
    "editor.snippetTabstopHighlightBackground" = base02;
    "editor.snippetTabstopHighlightBorder" = null;
    "editor.stackFrameHighlightBackground" = null;
    "editor.symbolHighlightBackground" = null;
    "editor.symbolHighlightBorder" = null;
    "editor.wordHighlightBackground" = base02;
    "editor.wordHighlightBorder" = null;
    "editor.wordHighlightStrongBackground" = base03;
    "editor.wordHighlightStrongBorder" = null;
    "editor.wordHighlightTextBackground" = null;
    "editor.wordHighlightTextBorder" = null;
    "editorActionList.background" = null;
    "editorActionList.focusBackground" = null;
    "editorActionList.focusForeground" = null;
    "editorActionList.foreground" = null;
    "editorBracketHighlight.foreground1" = base08;
    "editorBracketHighlight.foreground2" = base09;
    "editorBracketHighlight.foreground3" = base0A;
    "editorBracketHighlight.foreground4" = base0B;
    "editorBracketHighlight.foreground5" = base0D;
    "editorBracketHighlight.foreground6" = base0E;
    "editorBracketHighlight.unexpectedBracket.foreground" = base0F;
    "editorBracketMatch.background" = base02;
    "editorBracketMatch.border" = null;
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
    "editorCodeLens.foreground" = base02;
    "editorCommentsWidget.rangeActiveBackground" = null;
    "editorCommentsWidget.rangeBackground" = null;
    "editorCommentsWidget.replyInputBackground" = null;
    "editorCommentsWidget.resolvedBorder" = null;
    "editorCommentsWidget.unresolvedBorder" = null;
    "editorCursor.background" = null;
    "editorCursor.foreground" = base05;
    "editorError.background" = null;
    "editorError.border" = null;
    "editorError.foreground" = base08;
    "editorGhostText.background" = "#00000000";
    "editorGhostText.border" = null;
    "editorGhostText.foreground" = base03;
    "editorGroup.border" = null;
    "editorGroup.dropBackground" = "${base03}66";
    "editorGroup.dropIntoPromptBackground" = base00;
    "editorGroup.dropIntoPromptBorder" = null;
    "editorGroup.dropIntoPromptForeground" = base06;
    "editorGroup.emptyBackground" = base00;
    "editorGroup.focusedEmptyBorder" = base0D;
    "editorGroupHeader.border" = null;
    "editorGroupHeader.noTabsBackground" = base01;
    "editorGroupHeader.tabsBackground" = base01;
    "editorGroupHeader.tabsBorder" = null;
    "editorGutter.addedBackground" = base0B;
    "editorGutter.background" = base00;
    "editorGutter.commentGlyphForeground" = null;
    "editorGutter.commentRangeForeground" = base04;
    "editorGutter.commentUnresolvedGlyphForeground" = null;
    "editorGutter.deletedBackground" = base08;
    "editorGutter.foldingControlForeground" = base05;
    "editorGutter.modifiedBackground" = base0E;
    "editorHint.border" = null;
    "editorHint.foreground" = base0D;
    "editorHoverWidget.background" = base01;
    "editorHoverWidget.border" = base02;
    "editorHoverWidget.foreground" = base05;
    "editorHoverWidget.highlightForeground" = base0D;
    "editorHoverWidget.statusBarBackground" = base01;
    "editorIndentGuide.activeBackground" = base02;
    "editorIndentGuide.activeBackground1" = null;
    "editorIndentGuide.activeBackground2" = null;
    "editorIndentGuide.activeBackground3" = null;
    "editorIndentGuide.activeBackground4" = null;
    "editorIndentGuide.activeBackground5" = null;
    "editorIndentGuide.activeBackground6" = null;
    "editorIndentGuide.background" = base02;
    "editorIndentGuide.background1" = null;
    "editorIndentGuide.background2" = null;
    "editorIndentGuide.background3" = null;
    "editorIndentGuide.background4" = null;
    "editorIndentGuide.background5" = null;
    "editorIndentGuide.background6" = null;
    "editorInfo.background" = null;
    "editorInfo.border" = null;
    "editorInfo.foreground" = base0C;
    "editorInlayHint.background" = base01;
    "editorInlayHint.foreground" = base03;
    "editorInlayHint.parameterBackground" = base01;
    "editorInlayHint.parameterForeground" = base03;
    "editorInlayHint.typeBackground" = base01;
    "editorInlayHint.typeForeground" = base03;
    "editorLightBulb.foreground" = base0A;
    "editorLightBulbAi.foreground" = null;
    "editorLightBulbAutoFix.foreground" = base0D;
    "editorLineNumber.activeForeground" = base04;
    "editorLineNumber.dimmedForeground" = null;
    "editorLineNumber.foreground" = base03;
    "editorLink.activeForeground" = base0D;
    "editorMarkerNavigation.background" = base01;
    "editorMarkerNavigationError.background" = base08;
    "editorMarkerNavigationError.headerBackground" = "${base08}20";
    "editorMarkerNavigationInfo.background" = base0D;
    "editorMarkerNavigationInfo.headerBackground" = "${base0C}20";
    "editorMarkerNavigationWarning.background" = base0A;
    "editorMarkerNavigationWarning.headerBackground" = "${base0A}20";
    "editorMinimap.inlineChatInserted" = null;
    "editorMultiCursor.primary.background" = null;
    "editorMultiCursor.primary.foreground" = null;
    "editorMultiCursor.secondary.background" = null;
    "editorMultiCursor.secondary.foreground" = null;
    "editorOverviewRuler.addedForeground" = base0B;
    "editorOverviewRuler.background" = null;
    "editorOverviewRuler.border" = "#00000000";
    "editorOverviewRuler.bracketMatchForeground" = base06;
    "editorOverviewRuler.commentForeground" = null;
    "editorOverviewRuler.commentUnresolvedForeground" = null;
    "editorOverviewRuler.commonContentForeground" = base0F;
    "editorOverviewRuler.currentContentForeground" = base0D;
    "editorOverviewRuler.deletedForeground" = base08;
    "editorOverviewRuler.errorForeground" = base08;
    "editorOverviewRuler.findMatchForeground" = base0A;
    "editorOverviewRuler.incomingContentForeground" = base0B;
    "editorOverviewRuler.infoForeground" = base0C;
    "editorOverviewRuler.inlineChatInserted" = null;
    "editorOverviewRuler.inlineChatRemoved" = null;
    "editorOverviewRuler.modifiedForeground" = base0E;
    "editorOverviewRuler.rangeHighlightForeground" = base03;
    "editorOverviewRuler.selectionHighlightForeground" = base02;
    "editorOverviewRuler.warningForeground" = base0A;
    "editorOverviewRuler.wordHighlightForeground" = base07;
    "editorOverviewRuler.wordHighlightStrongForeground" = base0D;
    "editorOverviewRuler.wordHighlightTextForeground" = null;
    "editorPane.background" = base00;
    "editorRuler.foreground" = base02;
    "editorStickyScroll.background" = base00;
    "editorStickyScroll.border" = base02;
    "editorStickyScroll.shadow" = "#00000000";
    "editorStickyScrollHover.background" = base01;
    "editorSuggestWidget.background" = base01;
    "editorSuggestWidget.border" = base02;
    "editorSuggestWidget.focusHighlightForeground" = base0D;
    "editorSuggestWidget.foreground" = base05;
    "editorSuggestWidget.highlightForeground" = base0D;
    "editorSuggestWidget.selectedBackground" = base02;
    "editorSuggestWidget.selectedForeground" = base05;
    "editorSuggestWidget.selectedIconForeground" = base05;
    "editorSuggestWidgetStatus.foreground" = null;
    "editorUnicodeHighlight.background" = null;
    "editorUnicodeHighlight.border" = null;
    "editorUnnecessaryCode.border" = null;
    "editorUnnecessaryCode.opacity" = null;
    "editorWarning.background" = null;
    "editorWarning.border" = null;
    "editorWarning.foreground" = base0A;
    "editorWatermark.foreground" = null;
    "editorWhitespace.foreground" = base03;
    "editorWidget.background" = base00;
    "editorWidget.border" = base02;
    "editorWidget.foreground" = base05;
    "editorWidget.resizeBorder" = base0D;
    "errorForeground" = base08;
    "extensionBadge.remoteBackground" = base09;
    "extensionBadge.remoteForeground" = base07;
    "extensionButton.background" = base0D;
    "extensionButton.foreground" = base00;
    "extensionButton.hoverBackground" = "${base0D}C0";
    "extensionButton.prominentBackground" = base0B;
    "extensionButton.prominentForeground" = base00;
    "extensionButton.prominentHoverBackground" = "${base0B}C0";
    "extensionButton.separator" = "#00000000";
    "extensionIcon.preReleaseForeground" = base09;
    "extensionIcon.sponsorForeground" = null;
    "extensionIcon.starForeground" = base0A;
    "extensionIcon.verifiedForeground" = base0D;
    "focusBorder" = base0D;
    "foreground" = base05;
    "gauge.background" = null;
    "gauge.border" = null;
    "gauge.errorBackground" = null;
    "gauge.errorForeground" = null;
    "gauge.foreground" = null;
    "gauge.warningBackground" = null;
    "gauge.warningForeground" = null;
    "git.blame.editorDecorationForeground" = null;
    "gitDecoration.addedResourceForeground" = base0B;
    "gitDecoration.conflictingResourceForeground" = base0A;
    "gitDecoration.deletedResourceForeground" = base08;
    "gitDecoration.ignoredResourceForeground" = base03;
    "gitDecoration.modifiedResourceForeground" = base0E;
    "gitDecoration.renamedResourceForeground" = base0C;
    "gitDecoration.stageDeletedResourceForeground" = base08;
    "gitDecoration.stageModifiedResourceForeground" = base0E;
    "gitDecoration.submoduleResourceForeground" = base0F;
    "gitDecoration.untrackedResourceForeground" = base09;
    "icon.foreground" = base05;
    "inlineChat.background" = base01;
    "inlineChat.border" = base02;
    "inlineChat.foreground" = null;
    "inlineChat.shadow" = "#00000000";
    "inlineChatDiff.inserted" = "${base0B}60";
    "inlineChatDiff.removed" = "${base08}60";
    "inlineChatInput.background" = base00;
    "inlineChatInput.border" = base02;
    "inlineChatInput.focusBorder" = base0D;
    "inlineChatInput.placeholderForeground" = base03;
    "inlineEdit.gutterIndicator.background" = null;
    "inlineEdit.gutterIndicator.primaryBackground" = null;
    "inlineEdit.gutterIndicator.primaryForeground" = null;
    "inlineEdit.gutterIndicator.secondaryBackground" = null;
    "inlineEdit.gutterIndicator.secondaryForeground" = null;
    "inlineEdit.gutterIndicator.successfulBackground" = null;
    "inlineEdit.gutterIndicator.successfulForeground" = null;
    "inlineEdit.indicator.background" = null;
    "inlineEdit.indicator.border" = null;
    "inlineEdit.indicator.foreground" = null;
    "inlineEdit.modifiedBackground" = null;
    "inlineEdit.modifiedBorder" = null;
    "inlineEdit.modifiedChangedLineBackground" = null;
    "inlineEdit.modifiedChangedTextBackground" = null;
    "inlineEdit.originalBackground" = null;
    "inlineEdit.originalBorder" = null;
    "inlineEdit.originalChangedLineBackground" = null;
    "inlineEdit.originalChangedTextBackground" = null;
    "inlineEdit.tabWillAcceptBorder" = null;
    "inlineEdit.wordReplacementView.background" = null;
    "input.background" = base00;
    "input.border" = null;
    "input.foreground" = base05;
    "input.placeholderForeground" = base03;
    "inputOption.activeBackground" = base0D;
    "inputOption.activeBorder" = null;
    "inputOption.activeForeground" = base00;
    "inputOption.hoverBackground" = null;
    "inputValidation.errorBackground" = base08;
    "inputValidation.errorBorder" = base08;
    "inputValidation.errorForeground" = base05;
    "inputValidation.infoBackground" = base0D;
    "inputValidation.infoBorder" = base0D;
    "inputValidation.infoForeground" = base05;
    "inputValidation.warningBackground" = base0A;
    "inputValidation.warningBorder" = base0A;
    "inputValidation.warningForeground" = base05;
    "interactive.activeCodeBorder" = null;
    "interactive.inactiveCodeBorder" = null;
    "keybindingLabel.background" = base02;
    "keybindingLabel.border" = null;
    "keybindingLabel.bottomBorder" = base02;
    "keybindingLabel.foreground" = base05;
    "keybindingTable.headerBackground" = base02;
    "keybindingTable.rowsBackground" = base01;
    "list.activeSelectionBackground" = base02;
    "list.activeSelectionForeground" = base05;
    "list.activeSelectionIconForeground" = null;
    "list.deemphasizedForeground" = null;
    "list.dropBackground" = "${base03}66";
    "list.dropBetweenBackground" = null;
    "list.errorForeground" = base08;
    "list.filterMatchBackground" = base02;
    "list.filterMatchBorder" = null;
    "list.focusAndSelectionOutline" = base0D;
    "list.focusBackground" = base02;
    "list.focusForeground" = base05;
    "list.focusHighlightForeground" = null;
    "list.focusOutline" = base0D;
    "list.highlightForeground" = base07;
    "list.hoverBackground" = base02;
    "list.hoverForeground" = base05;
    "list.inactiveFocusBackground" = base02;
    "list.inactiveFocusOutline" = base03;
    "list.inactiveSelectionBackground" = base02;
    "list.inactiveSelectionForeground" = base05;
    "list.inactiveSelectionIconForeground" = null;
    "list.invalidItemForeground" = base08;
    "list.warningForeground" = base0A;
    "listFilterWidget.background" = base00;
    "listFilterWidget.noMatchesOutline" = base08;
    "listFilterWidget.outline" = null;
    "listFilterWidget.shadow" = "#00000000";
    "menu.background" = base01;
    "menu.border" = base02;
    "menu.foreground" = base05;
    "menu.selectionBackground" = base02;
    "menu.selectionBorder" = null;
    "menu.selectionForeground" = base05;
    "menu.separatorBackground" = base02;
    "menubar.selectionBackground" = base02;
    "menubar.selectionBorder" = null;
    "menubar.selectionForeground" = base05;
    "merge.border" = null;
    "merge.commonContentBackground" = null;
    "merge.commonHeaderBackground" = null;
    "merge.currentContentBackground" = "${base0D}18";
    "merge.currentHeaderBackground" = "${base0D}66";
    "merge.incomingContentBackground" = "${base0B}18";
    "merge.incomingHeaderBackground" = "${base0B}66";
    "mergeEditor.change.background" = null;
    "mergeEditor.change.word.background" = null;
    "mergeEditor.changeBase.background" = null;
    "mergeEditor.changeBase.word.background" = null;
    "mergeEditor.conflict.handled.minimapOverViewRuler" = null;
    "mergeEditor.conflict.handledFocused.border" = base0D;
    "mergeEditor.conflict.handledUnfocused.border" = null;
    "mergeEditor.conflict.input1.background" = null;
    "mergeEditor.conflict.input2.background" = null;
    "mergeEditor.conflict.unhandled.minimapOverViewRuler" = null;
    "mergeEditor.conflict.unhandledFocused.border" = base0D;
    "mergeEditor.conflict.unhandledUnfocused.border" = null;
    "mergeEditor.conflictingLines.background" = null;
    "minimap.background" = base00;
    "minimap.chatEditHighlight" = null;
    "minimap.errorHighlight" = base08;
    "minimap.findMatchHighlight" = base0A;
    "minimap.foregroundOpacity" = null;
    "minimap.infoHighlight" = null;
    "minimap.selectionHighlight" = base02;
    "minimap.selectionOccurrenceHighlight" = base03;
    "minimap.warningHighlight" = base0A;
    "minimapGutter.addedBackground" = base0B;
    "minimapGutter.deletedBackground" = base08;
    "minimapGutter.modifiedBackground" = base0E;
    "minimapSlider.activeBackground" = null;
    "minimapSlider.background" = null;
    "minimapSlider.hoverBackground" = null;
    "multiDiffEditor.background" = null;
    "multiDiffEditor.border" = null;
    "multiDiffEditor.headerBackground" = null;
    "notebook.cellBorderColor" = base03;
    "notebook.cellEditorBackground" = base00;
    "notebook.cellHoverBackground" = base01;
    "notebook.cellInsertionIndicator" = null;
    "notebook.cellStatusBarItemHoverBackground" = null;
    "notebook.cellToolbarSeparator" = base02;
    "notebook.editorBackground" = base00;
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
    "notebookEditorOverviewRuler.runningCellForeground" = null;
    "notebookScrollbarSlider.activeBackground" = null;
    "notebookScrollbarSlider.background" = null;
    "notebookScrollbarSlider.hoverBackground" = null;
    "notebookStatusErrorIcon.foreground" = base08;
    "notebookStatusRunningIcon.foreground" = base0C;
    "notebookStatusSuccessIcon.foreground" = base0B;
    "notificationCenter.border" = null;
    "notificationCenterHeader.background" = base01;
    "notificationCenterHeader.foreground" = base05;
    "notificationLink.foreground" = base0D;
    "notificationToast.border" = null;
    "notifications.background" = base02;
    "notifications.border" = null;
    "notifications.foreground" = base05;
    "notificationsErrorIcon.foreground" = base08;
    "notificationsInfoIcon.foreground" = base0D;
    "notificationsWarningIcon.foreground" = base0A;
    "outputView.background" = null;
    "outputViewStickyScroll.background" = null;
    "panel.background" = base01;
    "panel.border" = "#00000000";
    "panel.dropBorder" = base01;
    "panelInput.border" = null;
    "panelSection.border" = null;
    "panelSection.dropBackground" = null;
    "panelSectionHeader.background" = null;
    "panelSectionHeader.border" = null;
    "panelSectionHeader.foreground" = null;
    "panelStickyScroll.background" = null;
    "panelStickyScroll.border" = null;
    "panelStickyScroll.shadow" = null;
    "panelTitle.activeBorder" = null;
    "panelTitle.activeForeground" = base05;
    "panelTitle.border" = null;
    "panelTitle.inactiveForeground" = base03;
    "panelTitleBadge.background" = null;
    "panelTitleBadge.foreground" = null;
    "peekView.border" = null;
    "peekViewEditor.background" = base01;
    "peekViewEditor.matchHighlightBackground" = base09;
    "peekViewEditor.matchHighlightBorder" = null;
    "peekViewEditorGutter.background" = base01;
    "peekViewEditorStickyScroll.background" = null;
    "peekViewResult.background" = base00;
    "peekViewResult.fileForeground" = base05;
    "peekViewResult.lineForeground" = base03;
    "peekViewResult.matchHighlightBackground" = base09;
    "peekViewResult.selectionBackground" = base02;
    "peekViewResult.selectionForeground" = base05;
    "peekViewTitle.background" = base02;
    "peekViewTitleDescription.foreground" = base03;
    "peekViewTitleLabel.foreground" = base05;
    "pickerGroup.border" = base02;
    "pickerGroup.foreground" = base03;
    "ports.iconRunningProcessForeground" = base09;
    "problemsErrorIcon.foreground" = base08;
    "problemsInfoIcon.foreground" = base0C;
    "problemsWarningIcon.foreground" = base0A;
    "profileBadge.background" = base01;
    "profileBadge.foreground" = base03;
    "profiles.sashBorder" = null;
    "progressBar.background" = base03;
    "quickInput.background" = base01;
    "quickInput.foreground" = base05;
    "quickInputList.focusBackground" = base02;
    "quickInputList.focusForeground" = base05;
    "quickInputList.focusIconForeground" = base05;
    "quickInputTitle.background" = base01;
    "radio.activeBackground" = null;
    "radio.activeBorder" = null;
    "radio.activeForeground" = null;
    "radio.inactiveBackground" = null;
    "radio.inactiveBorder" = null;
    "radio.inactiveForeground" = null;
    "radio.inactiveHoverBackground" = null;
    "sash.hoverBorder" = null;
    "scmGraph.foreground1" = null;
    "scmGraph.foreground2" = null;
    "scmGraph.foreground3" = null;
    "scmGraph.foreground4" = null;
    "scmGraph.foreground5" = null;
    "scmGraph.historyItemBaseRefColor" = null;
    "scmGraph.historyItemHoverAdditionsForeground" = null;
    "scmGraph.historyItemHoverDefaultLabelBackground" = null;
    "scmGraph.historyItemHoverDefaultLabelForeground" = null;
    "scmGraph.historyItemHoverDeletionsForeground" = null;
    "scmGraph.historyItemHoverLabelForeground" = null;
    "scmGraph.historyItemRefColor" = null;
    "scmGraph.historyItemRemoteRefColor" = null;
    "scrollbar.shadow" = "#00000000";
    "scrollbarSlider.activeBackground" = "${base04}77";
    "scrollbarSlider.background" = "${base03}33";
    "scrollbarSlider.hoverBackground" = "${base03}77";
    "search.resultsInfoForeground" = null;
    "searchEditor.findMatchBackground" = "${base0A}99";
    "searchEditor.findMatchBorder" = null;
    "searchEditor.textInputBorder" = null;
    "selection.background" = base02;
    "settings.checkboxBackground" = base01;
    "settings.checkboxBorder" = null;
    "settings.checkboxForeground" = base05;
    "settings.dropdownBackground" = base01;
    "settings.dropdownBorder" = null;
    "settings.dropdownForeground" = base05;
    "settings.dropdownListBorder" = null;
    "settings.focusedRowBackground" = base02;
    "settings.focusedRowBorder" = base0D;
    "settings.headerBorder" = base05;
    "settings.headerForeground" = base05;
    "settings.modifiedItemIndicator" = base0D;
    "settings.numberInputBackground" = base01;
    "settings.numberInputBorder" = null;
    "settings.numberInputForeground" = base05;
    "settings.rowHoverBackground" = base02;
    "settings.sashBorder" = base05;
    "settings.settingsHeaderHoverForeground" = null;
    "settings.textInputBackground" = base01;
    "settings.textInputBorder" = null;
    "settings.textInputForeground" = base05;
    "sideBar.background" = base01;
    "sideBar.border" = null;
    "sideBar.dropBackground" = base02;
    "sideBar.foreground" = base05;
    "sideBarActivityBarTop.border" = null;
    "sideBarSectionHeader.background" = base01;
    "sideBarSectionHeader.border" = null;
    "sideBarSectionHeader.foreground" = base05;
    "sideBarStickyScroll.background" = null;
    "sideBarStickyScroll.border" = null;
    "sideBarStickyScroll.shadow" = null;
    "sideBarTitle.background" = null;
    "sideBarTitle.border" = null;
    "sideBarTitle.foreground" = base05;
    "sideBySideEditor.horizontalBorder" = null;
    "sideBySideEditor.verticalBorder" = null;
    "simpleFindWidget.sashBorder" = null;
    "statusBar.background" = base01;
    "statusBar.border" = null;
    "statusBar.debuggingBackground" = base09;
    "statusBar.debuggingBorder" = null;
    "statusBar.debuggingForeground" = base00;
    "statusBar.focusBorder" = base0D;
    "statusBar.foreground" = base05;
    "statusBar.noFolderBackground" = base01;
    "statusBar.noFolderBorder" = null;
    "statusBar.noFolderForeground" = base05;
    "statusBarItem.activeBackground" = base02;
    "statusBarItem.compactHoverBackground" = base02;
    "statusBarItem.errorBackground" = base08;
    "statusBarItem.errorForeground" = base00;
    "statusBarItem.errorHoverBackground" = "${base08}C0";
    "statusBarItem.errorHoverForeground" = base00;
    "statusBarItem.focusBorder" = base0D;
    "statusBarItem.hoverBackground" = base02;
    "statusBarItem.hoverForeground" = base05;
    "statusBarItem.offlineBackground" = base09;
    "statusBarItem.offlineForeground" = base00;
    "statusBarItem.offlineHoverBackground" = "${base09}C0";
    "statusBarItem.offlineHoverForeground" = base00;
    "statusBarItem.prominentBackground" = base0E;
    "statusBarItem.prominentForeground" = base00;
    "statusBarItem.prominentHoverBackground" = "${base0E}C0";
    "statusBarItem.prominentHoverForeground" = base00;
    "statusBarItem.remoteBackground" = base01;
    "statusBarItem.remoteForeground" = base05;
    "statusBarItem.remoteHoverBackground" = base02;
    "statusBarItem.remoteHoverForeground" = base05;
    "statusBarItem.warningBackground" = base0A;
    "statusBarItem.warningForeground" = base00;
    "statusBarItem.warningHoverBackground" = "${base0A}C0";
    "statusBarItem.warningHoverForeground" = base00;
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
    "tab.activeBackground" = base02;
    "tab.activeBorder" = null;
    "tab.activeBorderTop" = null;
    "tab.activeForeground" = base05;
    "tab.activeModifiedBorder" = base0D;
    "tab.border" = "#00000000";
    "tab.dragAndDropBorder" = base03;
    "tab.hoverBackground" = base02;
    "tab.hoverBorder" = null;
    "tab.hoverForeground" = base05;
    "tab.inactiveBackground" = base01;
    "tab.inactiveForeground" = base05;
    "tab.inactiveModifiedBorder" = base0D;
    "tab.lastPinnedBorder" = null;
    "tab.selectedBackground" = null;
    "tab.selectedBorderTop" = null;
    "tab.selectedForeground" = null;
    "tab.unfocusedActiveBackground" = base02;
    "tab.unfocusedActiveBorder" = null;
    "tab.unfocusedActiveBorderTop" = null;
    "tab.unfocusedActiveForeground" = base04;
    "tab.unfocusedActiveModifiedBorder" = base0D;
    "tab.unfocusedHoverBackground" = base02;
    "tab.unfocusedHoverBorder" = null;
    "tab.unfocusedHoverForeground" = base05;
    "tab.unfocusedInactiveBackground" = base01;
    "tab.unfocusedInactiveForeground" = base04;
    "tab.unfocusedInactiveModifiedBorder" = base0D;
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
    "terminal.background" = base00;
    "terminal.border" = null;
    "terminal.dropBackground" = null;
    "terminal.findMatchBackground" = null;
    "terminal.findMatchBorder" = null;
    "terminal.findMatchHighlightBackground" = null;
    "terminal.findMatchHighlightBorder" = null;
    "terminal.foreground" = base05;
    "terminal.hoverHighlightBackground" = null;
    "terminal.inactiveSelectionBackground" = null;
    "terminal.initialHintForeground" = null;
    "terminal.selectionBackground" = null;
    "terminal.selectionForeground" = null;
    "terminal.tab.activeBorder" = null;
    "terminalCommandDecoration.defaultBackground" = null;
    "terminalCommandDecoration.errorBackground" = null;
    "terminalCommandDecoration.successBackground" = null;
    "terminalCommandGuide.foreground" = null;
    "terminalCursor.background" = null;
    "terminalCursor.foreground" = base05;
    "terminalOverviewRuler.border" = null;
    "terminalOverviewRuler.cursorForeground" = "#ff0000";
    "terminalOverviewRuler.findMatchForeground" = "#ff0000";
    "terminalStickyScroll.background" = null;
    "terminalStickyScroll.border" = null;
    "terminalStickyScrollHover.background" = null;
    "terminalSymbolIcon.aliasForeground" = null;
    "terminalSymbolIcon.flagForeground" = null;
    "testing.coverCountBadgeBackground" = null;
    "testing.coverCountBadgeForeground" = null;
    "testing.coveredBackground" = null;
    "testing.coveredBorder" = null;
    "testing.coveredGutterBackground" = null;
    "testing.iconErrored" = base0F;
    "testing.iconErrored.retired" = null;
    "testing.iconFailed" = base08;
    "testing.iconFailed.retired" = null;
    "testing.iconPassed" = base0B;
    "testing.iconPassed.retired" = null;
    "testing.iconQueued" = base0A;
    "testing.iconQueued.retired" = null;
    "testing.iconSkipped" = base0E;
    "testing.iconSkipped.retired" = null;
    "testing.iconUnset" = base04;
    "testing.iconUnset.retired" = null;
    "testing.message.error.badgeBackground" = null;
    "testing.message.error.badgeBorder" = null;
    "testing.message.error.badgeForeground" = null;
    "testing.message.error.lineBackground" = "${base08}20";
    "testing.message.info.decorationForeground" = base05;
    "testing.message.info.lineBackground" = "${base0D}20";
    "testing.messagePeekBorder" = null;
    "testing.messagePeekHeaderBackground" = null;
    "testing.peekBorder" = null;
    "testing.peekHeaderBackground" = base01;
    "testing.runAction" = base04;
    "testing.uncoveredBackground" = null;
    "testing.uncoveredBorder" = null;
    "testing.uncoveredBranchBackground" = null;
    "testing.uncoveredGutterBackground" = null;
    "textBlockQuote.background" = base01;
    "textBlockQuote.border" = base0D;
    "textCodeBlock.background" = base00;
    "textLink.activeForeground" = base0C;
    "textLink.foreground" = base0D;
    "textPreformat.background" = null;
    "textPreformat.foreground" = base0D;
    "textSeparator.foreground" = base05;
    "titleBar.activeBackground" = base01;
    "titleBar.activeForeground" = base05;
    "titleBar.border" = null;
    "titleBar.inactiveBackground" = base01;
    "titleBar.inactiveForeground" = base03;
    "toolbar.activeBackground" = base02;
    "toolbar.hoverBackground" = base02;
    "toolbar.hoverOutline" = null;
    "tree.inactiveIndentGuidesStroke" = null;
    "tree.indentGuidesStroke" = base05;
    "tree.tableColumnsBorder" = null;
    "tree.tableOddRowsBackground" = null;
    "walkThrough.embeddedEditorBackground" = base00;
    "walkthrough.stepTitle.foreground" = null;
    "welcomePage.background" = base00;
    "welcomePage.progress.background" = base03;
    "welcomePage.progress.foreground" = base0D;
    "welcomePage.tileBackground" = base01;
    "welcomePage.tileBorder" = null;
    "welcomePage.tileHoverBackground" = base02;
    "widget.border" = base02;
    "widget.shadow" = "#00000000";
    "window.activeBorder" = null;
    "window.inactiveBorder" = null;
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
