(in-package :monkeylib-gsuite)

(defschema auto-text "textStyle" "suggestedTextStyleChanges" "type" "suggestedInsertionIds" "suggestedDeletionIds")

(defschema background "color")

(defschema background-suggestion-state "backgroundColorSuggested")

(defschema batch-update-document-request "requests" "writeControl")

(defschema batch-update-document-response "writeControl" "replies" "documentId")

(defschema body "content")

(defschema bullet "textStyle" "nestingLevel" "listId")

(defschema bullet-suggestion-state "nestingLevelSuggested" "textStyleSuggestionState" "listIdSuggested")

(defschema color "rgbColor")

(defschema column-break "suggestedInsertionIds" "suggestedDeletionIds" "suggestedTextStyleChanges" "textStyle")

(defschema create-footer-request "type" "sectionBreakLocation")

(defschema create-footer-response "footerId")

(defschema create-footnote-request "location" "endOfSegmentLocation")

(defschema create-footnote-response "footnoteId")

(defschema create-header-request "sectionBreakLocation" "type")

(defschema create-header-response "headerId")

(defschema create-named-range-request "range" "name")

(defschema create-named-range-response "namedRangeId")

(defschema create-paragraph-bullets-request "range" "bulletPreset")

(defschema crop-properties "offsetTop" "offsetRight" "angle" "offsetBottom" "offsetLeft")

(defschema crop-properties-suggestion-state "angleSuggested" "offsetLeftSuggested" "offsetBottomSuggested"
           "offsetTopSuggested" "offsetRightSuggested")

(defschema delete-content-range-request "range")

(defschema delete-footer-request "footerId")

(defschema delete-header-request "headerId")

(defschema delete-named-range-request "namedRangeId" "name")

(defschema delete-paragraph-bullets-request "range")

(defschema delete-positioned-object-request "objectId")

(defschema delete-table-column-request "tableCellLocation")

(defschema delete-table-row-request "tableCellLocation")

(defschema dimension "unit" "magnitude")

(defschema document "title" "lists" "suggestedDocumentStyleChanges" "inlineObjects" "suggestedNamedStylesChanges"
           "documentId" "documentStyle" "footnotes" "headers" "namedRanges" "footers" "revisionId" "body" "namedStyles"
           "positionedObjects" "suggestionsViewMode")

(defschema document-style "marginTop" "pageNumberStart" "firstPageHeaderId" "marginBottom" "marginRight" "marginFooter"
           "firstPageFooterId" "marginHeader" "evenPageFooterId" "pageSize" "evenPageHeaderId"
           "useCustomHeaderFooterMargins" "useFirstPageHeaderFooter" "useEvenPageHeaderFooter" "defaultHeaderId"
           "background" "defaultFooterId" "marginLeft")

(defschema document-style-suggestion-state "firstPageFooterIdSuggested" "useEvenPageHeaderFooterSuggested"
           "defaultHeaderIdSuggested" "marginBottomSuggested" "marginRightSuggested" "marginTopSuggested"
           "defaultFooterIdSuggested" "useCustomHeaderFooterMarginsSuggested" "backgroundSuggestionState"
           "pageNumberStartSuggested" "marginFooterSuggested" "evenPageFooterIdSuggested" "pageSizeSuggestionState"
           "marginHeaderSuggested" "evenPageHeaderIdSuggested" "marginLeftSuggested" "firstPageHeaderIdSuggested"
           "useFirstPageHeaderFooterSuggested")

(defschema embedded-drawing-properties)

(defschema embedded-drawing-properties-suggestion-state)

(defschema embedded-object "title" "size" "linkedContentReference" "imageProperties" "marginLeft" "marginBottom"
           "marginRight" "marginTop" "embeddedObjectBorder" "embeddedDrawingProperties" "description")

(defschema embedded-object-border "propertyState" "color" "width" "dashStyle")

(defschema embedded-object-border-suggestion-state "widthSuggested" "dashStyleSuggested" "colorSuggested"
           "propertyStateSuggested")

(defschema embedded-object-suggestion-state "linkedContentReferenceSuggestionState" "marginRightSuggested"
           "sizeSuggestionState" "embeddedObjectBorderSuggestionState" "imagePropertiesSuggestionState"
           "marginTopSuggested" "descriptionSuggested" "embeddedDrawingPropertiesSuggestionState" "titleSuggested"
           "marginLeftSuggested" "marginBottomSuggested")

(defschema end-of-segment-location "segmentId")

(defschema equation "suggestedInsertionIds" "suggestedDeletionIds")

(defschema footer "footerId" "content")

(defschema footnote "content" "footnoteId")

(defschema footnote-reference "suggestedDeletionIds" "suggestedInsertionIds" "footnoteId" "footnoteNumber" "textStyle"
           "suggestedTextStyleChanges")

(defschema header "content" "headerId")

(defschema horizontal-rule "suggestedInsertionIds" "suggestedDeletionIds" "suggestedTextStyleChanges" "textStyle")

(defschema image-properties "contrast" "brightness" "sourceUri" "transparency" "angle" "cropProperties" "contentUri")

(defschema image-properties-suggestion-state "sourceUriSuggested" "contrastSuggested" "cropPropertiesSuggestionState"
           "brightnessSuggested" "contentUriSuggested" "angleSuggested" "transparencySuggested")

(defschema inline-object "objectId" "suggestedDeletionIds" "inlineObjectProperties" "suggestedInsertionId"
           "suggestedInlineObjectPropertiesChanges")

(defschema inline-object-element "suggestedDeletionIds" "suggestedInsertionIds" "suggestedTextStyleChanges" "textStyle"
           "inlineObjectId")

(defschema inline-object-properties "embeddedObject")

(defschema inline-object-properties-suggestion-state "embeddedObjectSuggestionState")

(defschema insert-inline-image-request "uri" "endOfSegmentLocation" "objectSize" "location")

(defschema insert-inline-image-response "objectId")

(defschema insert-inline-sheets-chart-response "objectId")

(defschema insert-page-break-request "endOfSegmentLocation" "location")

(defschema insert-section-break-request "endOfSegmentLocation" "location" "sectionType")

(defschema insert-table-column-request "tableCellLocation" "insertRight")

(defschema insert-table-request "columns" "location" "rows" "endOfSegmentLocation")

(defschema insert-table-row-request "insertBelow" "tableCellLocation")

(defschema insert-text-request "endOfSegmentLocation" "text" "location")

(defschema link "bookmarkId" "url" "headingId")

(defschema linked-content-reference "sheetsChartReference")

(defschema linked-content-reference-suggestion-state "sheetsChartReferenceSuggestionState")

(defschema list "suggestedInsertionId" "suggestedListPropertiesChanges" "suggestedDeletionIds" "listProperties")

(defschema list-properties "nestingLevels")

(defschema list-properties-suggestion-state "nestingLevelsSuggestionStates")

(defschema location "segmentId" "index")

(defschema merge-table-cells-request "tableRange")

(defschema named-range "name" "namedRangeId" "ranges")

(defschema named-ranges "namedRanges" "name")

(defschema named-style "textStyle" "namedStyleType" "paragraphStyle")

(defschema named-style-suggestion-state "paragraphStyleSuggestionState" "namedStyleType" "textStyleSuggestionState")

(defschema named-styles "styles")

(defschema named-styles-suggestion-state "stylesSuggestionStates")

(defschema nesting-level "textStyle" "indentFirstLine" "glyphType" "startNumber" "indentStart" "bulletAlignment"
           "glyphSymbol" "glyphFormat")

(defschema nesting-level-suggestion-state "glyphTypeSuggested" "startNumberSuggested" "indentStartSuggested"
           "glyphSymbolSuggested" "textStyleSuggestionState" "bulletAlignmentSuggested" "indentFirstLineSuggested"
           "glyphFormatSuggested")

(defschema object-references "objectIds")

(defschema optional-color "color")

(defschema page-break "suggestedDeletionIds" "suggestedInsertionIds" "suggestedTextStyleChanges" "textStyle")

(defschema paragraph "bullet" "suggestedBulletChanges" "paragraphStyle" "positionedObjectIds"
           "suggestedPositionedObjectIds" "suggestedParagraphStyleChanges" "elements")

(defschema paragraph-border "color" "padding" "dashStyle" "width")

(defschema paragraph-element "footnoteReference" "startIndex" "pageBreak" "inlineObjectElement" "columnBreak" "textRun"
           "autoText" "endIndex" "horizontalRule" "equation" "person")

(defschema paragraph-style "borderRight" "indentEnd" "indentFirstLine" "keepWithNext" "namedStyleType" "borderLeft"
           "spaceBelow" "spaceAbove" "headingId" "alignment" "tabStops" "lineSpacing" "indentStart" "direction"
           "shading" "borderBetween" "keepLinesTogether" "spacingMode" "borderBottom" "borderTop" "avoidWidowAndOrphan")

(defschema paragraph-style-suggestion-state "borderBottomSuggested" "avoidWidowAndOrphanSuggested" "borderTopSuggested"
           "lineSpacingSuggested" "keepLinesTogetherSuggested" "borderLeftSuggested" "keepWithNextSuggested"
           "borderRightSuggested" "indentFirstLineSuggested" "spaceAboveSuggested" "directionSuggested"
           "spacingModeSuggested" "spaceBelowSuggested" "borderBetweenSuggested" "indentEndSuggested"
           "shadingSuggestionState" "namedStyleTypeSuggested" "indentStartSuggested" "alignmentSuggested"
           "headingIdSuggested")

(defschema person "suggestedInsertionIds" "personProperties" "personId" "suggestedDeletionIds"
           "suggestedTextStyleChanges" "textStyle")

(defschema person-properties "email" "name")

(defschema positioned-object "suggestedPositionedObjectPropertiesChanges" "positionedObjectProperties"
           "suggestedDeletionIds" "suggestedInsertionId" "objectId")

(defschema positioned-object-positioning "topOffset" "leftOffset" "layout")

(defschema positioned-object-positioning-suggestion-state "leftOffsetSuggested" "layoutSuggested" "topOffsetSuggested")

(defschema positioned-object-properties "positioning" "embeddedObject")

(defschema positioned-object-properties-suggestion-state "positioningSuggestionState" "embeddedObjectSuggestionState")

(defschema range "startIndex" "segmentId" "endIndex")

(defschema replace-all-text-request "containsText" "replaceText")

(defschema replace-all-text-response "occurrencesChanged")

(defschema replace-image-request "uri" "imageObjectId" "imageReplaceMethod")

(defschema replace-named-range-content-request "namedRangeName" "text" "namedRangeId")

(defschema request "insertSectionBreak" "deleteHeader" "insertTableColumn" "insertPageBreak" "createNamedRange"
           "updateTableColumnProperties" "deleteContentRange" "replaceImage" "insertTable" "insertInlineImage"
           "insertText" "deleteTableColumn" "updateSectionStyle" "updateDocumentStyle" "updateTableRowStyle"
           "updateTextStyle" "deleteNamedRange" "deletePositionedObject" "unmergeTableCells" "deleteFooter"
           "mergeTableCells" "createParagraphBullets" "replaceAllText" "deleteTableRow" "deleteParagraphBullets"
           "createFootnote" "updateParagraphStyle" "createFooter" "createHeader" "insertTableRow"
           "updateTableCellStyle" "replaceNamedRangeContent")

(defschema response "replaceAllText" "createHeader" "createNamedRange" "createFooter" "insertInlineSheetsChart"
           "insertInlineImage" "createFootnote")

(defschema rgb-color "blue" "red" "green")

(defschema section-break "sectionStyle" "suggestedDeletionIds" "suggestedInsertionIds")

(defschema section-column-properties "width" "paddingEnd")

(defschema section-style "marginRight" "firstPageHeaderId" "columnProperties" "marginTop" "contentDirection"
           "marginBottom" "marginFooter" "evenPageHeaderId" "sectionType" "columnSeparatorStyle" "firstPageFooterId"
           "pageNumberStart" "evenPageFooterId" "marginHeader" "defaultHeaderId" "useFirstPageHeaderFooter"
           "defaultFooterId" "marginLeft")

(defschema shading "backgroundColor")

(defschema shading-suggestion-state "backgroundColorSuggested")

(defschema sheets-chart-reference "spreadsheetId" "chartId")

(defschema sheets-chart-reference-suggestion-state "chartIdSuggested" "spreadsheetIdSuggested")

(defschema size "height" "width")

(defschema size-suggestion-state "widthSuggested" "heightSuggested")

(defschema structural-element "table" "endIndex" "paragraph" "sectionBreak" "tableOfContents" "startIndex")

(defschema substring-match-criteria "text" "matchCase")

(defschema suggested-bullet "bulletSuggestionState" "bullet")

(defschema suggested-document-style "documentStyle" "documentStyleSuggestionState")

(defschema suggested-inline-object-properties "inlineObjectProperties" "inlineObjectPropertiesSuggestionState")

(defschema suggested-list-properties "listPropertiesSuggestionState" "listProperties")

(defschema suggested-named-styles "namedStyles" "namedStylesSuggestionState")

(defschema suggested-paragraph-style "paragraphStyle" "paragraphStyleSuggestionState")

(defschema suggested-positioned-object-properties "positionedObjectPropertiesSuggestionState"
           "positionedObjectProperties")

(defschema suggested-table-cell-style "tableCellStyle" "tableCellStyleSuggestionState")

(defschema suggested-table-row-style "tableRowStyle" "tableRowStyleSuggestionState")

(defschema suggested-text-style "textStyle" "textStyleSuggestionState")

(defschema tab-stop "alignment" "offset")

(defschema table "suggestedInsertionIds" "rows" "columns" "tableStyle" "suggestedDeletionIds" "tableRows")

(defschema table-cell "suggestedDeletionIds" "content" "endIndex" "suggestedInsertionIds"
           "suggestedTableCellStyleChanges" "startIndex" "tableCellStyle")

(defschema table-cell-border "width" "color" "dashStyle")

(defschema table-cell-location "columnIndex" "tableStartLocation" "rowIndex")

(defschema table-cell-style "paddingTop" "paddingLeft" "paddingBottom" "borderBottom" "backgroundColor" "columnSpan"
           "paddingRight" "borderTop" "rowSpan" "borderRight" "borderLeft" "contentAlignment")

(defschema table-cell-style-suggestion-state "columnSpanSuggested" "rowSpanSuggested" "contentAlignmentSuggested"
           "backgroundColorSuggested" "paddingTopSuggested" "borderTopSuggested" "paddingBottomSuggested"
           "paddingLeftSuggested" "paddingRightSuggested" "borderLeftSuggested" "borderRightSuggested"
           "borderBottomSuggested")

(defschema table-column-properties "width" "widthType")

(defschema table-of-contents "suggestedInsertionIds" "content" "suggestedDeletionIds")

(defschema table-range "columnSpan" "tableCellLocation" "rowSpan")

(defschema table-row "suggestedInsertionIds" "suggestedDeletionIds" "tableRowStyle" "tableCells" "startIndex"
           "endIndex" "suggestedTableRowStyleChanges")

(defschema table-row-style "minRowHeight")

(defschema table-row-style-suggestion-state "minRowHeightSuggested")

(defschema table-style "tableColumnProperties")

(defschema text-run "suggestedDeletionIds" "suggestedInsertionIds" "content" "suggestedTextStyleChanges" "textStyle")

(defschema text-style "strikethrough" "italic" "bold" "backgroundColor" "weightedFontFamily" "fontSize" "smallCaps"
           "underline" "link" "foregroundColor" "baselineOffset")

(defschema text-style-suggestion-state "strikethroughSuggested" "linkSuggested" "smallCapsSuggested"
           "weightedFontFamilySuggested" "foregroundColorSuggested" "underlineSuggested" "fontSizeSuggested"
           "baselineOffsetSuggested" "backgroundColorSuggested" "italicSuggested" "boldSuggested")

(defschema unmerge-table-cells-request "tableRange")

(defschema update-document-style-request "documentStyle" "fields")

(defschema update-paragraph-style-request "paragraphStyle" "range" "fields")

(defschema update-section-style-request "range" "sectionStyle" "fields")

(defschema update-table-cell-style-request "tableRange" "tableCellStyle" "fields" "tableStartLocation")

(defschema update-table-column-properties-request "fields" "columnIndices" "tableStartLocation" "tableColumnProperties")

(defschema update-table-row-style-request "rowIndices" "tableStartLocation" "fields" "tableRowStyle")

(defschema update-text-style-request "fields" "range" "textStyle")

(defschema weighted-font-family "fontFamily" "weight")

(defschema write-control "requiredRevisionId" "targetRevisionId")
