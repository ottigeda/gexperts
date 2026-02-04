# GExperts 1.3.26 Changes

Changes since version 1.3.25 (released 2024-08-24)

## New Features

### Delphi 13 (RAD Studio 13 Florence) Support
- Full support for Delphi 13 32-bit and 64-bit IDEs
- Added GExperts version info to the IDE's about screen
- Support for Delphi 13 ternary operator (inline if expressions) in the code formatter
- Support for Delphi 13 'noreturn' directive in the code formatter

### 64-bit IDE Support
- GExperts now supports 64-bit IDEs (Delphi 12.3+ and Delphi 13)
- Fixed pointer typecasts and registry key names for 64-bit IDE
- RAD Studio 13 installer now includes both 32-bit and 64-bit DLLs
- Optional checkbox to register for 64-bit IDE (checked by default)
- ExplicitFilter expert now works on Win64 (uses DDetours library instead of manual function hooking)

### New Experts
- **Auto Switch Desktop**: Automatically select a desktop when switching between code editor and form designer with F12 (Feature Request #196)
- **Editor Bookmarks**: New expert for managing editor bookmarks (contributed by Ralph Zornig)

### Uses Clause Manager Enhancements
- Drag & Drop support in the Identifier tab (Feature Request #194)
- Multi-select support in the Identifier tab (Feature Request #193)
- Fix for empty lines left behind when removing units (Bug #226)

### ASCII Chart Enhancements
- Code page selection for international characters (Arabic, Cyrillic, Hebrew, etc.) in Unicode Delphi versions (Bugs #164, #403, #254)

### IDE Search Path Editor
- Export and import for the search path favorites list (Feature Request #212)
- Added buttons to make paths absolute/relative and prepend/remove '../' from directories

### Instant Grep Improvements
- Uses IDE's Line Highlight background color for matches
- Searches for current identifier by default
- Can replace IDE's Find command when called via Ctrl+F

## Bug Fixes

### General
- Fixed Delphi 7 compile error (duplicate MinInt32 definition in dzlib)
- #360: Fixed scaling the list in the Go to dialog
- #375: Fixed blue bar selection in comboboxes
- #388: Fixed Code librarian font sizes not restored after IDE restart in Delphi 12
- #393: Fixed GExperts key mappings removed when unloading another ToolsAPI package
- #394: Fixed Rename Components dialog scaling for additional properties
- #399: Fixed Open File configuration dialog scaling
- #401: Fixed Class Browser always has a horizontal scroll bar
- #402: Fixed icons in Class Browser window do not scale properly
- #407: Fixed "..." button missing from Install Packages dialog in Delphi 12
- #408: Fixed Search dialog in Code Librarian scaling
- #410: Fixed Rename Components config dialog opens with Width and Height = 0
- #411: Fixed "List index out of bounds" when opening files in IDE
- #419: Fixed cut-off items in Favorite Files list
- #420: Fixed error with long path names calling PathCanonicalize
- #421: Fixed Insert IFDEF/IFNDEF has 4 empty rows on top
- #424: Fixed Bookmarks expert configuration dialog resizing
- #427/#460: Fixed Formatter capitalization list losing entries
- #428: Fixed index out of bounds when filtering identifiers in start first mode
- #436: Fixed duplicate inactive text in splash screen
- #439: Font scaling at 125% (partial fix)
- #441: Formatter: Remove unneeded note
- #443: Fixed IDE enhancements hiding Target label in Project Options
- #451: Fixed can't disable InstantGrep
- #453: Fixed format selected code indents the next line
- #454: Fixed IDE options for closing message window not persisted
- #462: Fixed statusbar update slowing down search time
- #463: Fixed Help-Button not moving when resizing Add to Favorite Search Path dialog

### Formatter Bugs
- #161: Fixed formatter not indenting multiline function directives
- #306: Fixed formatter spacing for generics property
- #322: Fixed formatter indents Index even if it is a normal field name
- #378: Fixed nested procedures in anonymous procedures with parameters
- #396: Fixed formatter no longer indents nested $IFDEFs (new independent mode)
- #404: Fixed formatter inserts break in {*)} comment
- #406: Fixed formatter malforming "operator" in Delphi 7
- #415: Fixed formatter adds double spacing for const values
- #418: Fixed various problems with wrong indentation after comments
- #425: Fixed formatter inserts empty line before external procedure declarations
- #426: Fixed wrong indentation after comment line(s)
- #435: Fixed formatting of nested function calls in if statement
- #442: Fixed line breaks before comma commenting out unit names in uses lists
- #444: Fixed formatter not indenting variant part of record with line break
- #445: Fixed formatter handles only 3 quotes for multiline strings
- #450: Fixed formatter not storing cursor position of additional edit windows
- #457: Fixed formatter inserts line feed between unit in uses and {$endif}

## Code Formatter

### New Options
- **IndentInterfaceImpl**: Indent content inside interface and implementation sections (Feature #203)
- **DoubleIndentIfCondition**: Double indent for multi-line if conditions (Feature #207)
- **BlankBeforeUses**: Insert blank line before uses clauses
- **IndentCompDirectives**: Extended to support independent directive nesting (icdIndependent mode)
- **UnindentLabels**: Option to control label indentation

### Delphi 13 Ternary Operator Support
- Full support for inline if expressions (ternary operator)
- Proper indentation for both single-line and vertical formatting
- Handles nested ternary expressions
- Support for ternary in various contexts (assignments, case statements, for loops, etc.)

### Breakpoint and Bookmark Preservation
- Formatter now restores breakpoints and bookmarks to their correct new line numbers after formatting
- Also preserves cursor position

### Configuration Dialog Improvements
- Context-sensitive preview: Each settings tab now shows a preview file demonstrating the relevant options
- Preview content is preserved when switching tabs after user modification
- "(edit or drop file)" hint shows users they can customize the preview
- "(reset)" link appears after modification to restore default preview for current tab

### Other Formatter Improvements
- Better handling of continuation indent after single-line comments
- Improved nested procedure indentation in anonymous procedures
- Better handling of compiler directives (inline {$IFDEF}...{$ENDIF})
- Tokenizer now recognizes {$IFEND}

## UI/UX Improvements

### Splitter Visibility
- Added visible splitter painting to all splitters in GExperts forms
- New IDE improvement option: Enhance Splitter Visibility

### Theming Support
- GUI adjustments for themed IDE
- Better handling of dark mode icons (merged from dark mode branch)
- Fixed checkbox and radiogroup heights for themed IDEs

### Dialog Improvements
- Message boxes now use read-only TMemo for better text selection (Feature #197)
- Improved DPI scaling across many dialogs
- Better form centering behavior

## Documentation

### Help Updates
- Updated documentation for Uses Clause Manager
- Added documentation for Editor Bookmarks expert
- Added documentation for Edit Unit Search Path expert
- Added rudimentary help for the Code Formatter
- Various help text corrections and improvements

## Internal Changes

- Build scripts: Added --nowait option to all __Build_Project.cmd scripts
- Build scripts: Added _buildproject.sh WSL scripts for building from Linux/WSL
- Updated to latest dzlib version
- Refactored GxOtaXxx functions to Ota.Xxx class methods
- Improved unit test framework for formatter
- Added console mode to formatter unit tests
- Many code cleanup and refactoring changes
- Fixed various compiler warnings for 64-bit builds
- Removed unused code and variables

## Contributors

- Thomas Mueller (twm)
- Achim Kalwa (akalwahome)
- Ralph Zornig (rzrz)
- Jim McKeeth (jimmckeeth)
- Patches from: Plamen Marinov, Stefan Glienke, Uwe Raabe, Uli Gerhardt, Christoph Schwerdtfeger, Benedikt Schindler, baka0815
