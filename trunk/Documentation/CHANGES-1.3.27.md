# GExperts 1.3.27 Changes

Changes since version 1.3.26 (released 2026-02-07)

## New Features

- Configuration dialog: Real-time warning when two enabled experts share the same keyboard shortcut, showing the shortcut and the conflicting expert names; updates immediately when shortcuts or enabled state change
- Configuration dialog: Fixed shortcut warning label being partially hidden behind button panel
- Code Formatter: Added option to use identifiers from Uses Clause Manager for capitalization (feature #218)
- Code Proofreader: Added option to use identifiers from Uses Clause Manager for case correction (feature #218)
- Code Formatter: Added option to align assignment operators (:=) at a fixed column position
- Reverse Statement: Replaced simple function name pairs with configurable regex-based reversal rules (feature #14)
- Reverse Statement: Default rules now correctly handle Read/Write method transformations (e.g. ReadString/WriteString)
- Reverse Statement: Added configuration dialog with interactive Test button
- Reverse Statement: Redesigned configuration dialog with two-column grid (match/replacement) replacing raw text editor (feature #14)
- Reverse Statement: Added edit sub-dialog for adding/editing individual reversal rules with real-time regex validation and test (feature #14)
- Reverse Statement: Moved source files to dedicated Source/ReverseStatement/ directory (feature #14)
- DfmFilter: New standalone command-line tool to detect and remove Delphi 2006+ Margins/AlignWithMargins properties from DFM files
- Uses Clause Manager: Added core sorting/grouping logic for uses clauses by namespace prefix (feature #181, feature #128)
- Uses Clause Manager: Added Sort button and Auto Sort checkbox to the dialog (feature #181)
- Uses Clause Manager: Made prefix groups and separator style configurable in the options dialog (feature #181)
- ShowGxMessageBox: Added optional owner parameter for centering on parent forms
- GxInspect: Added TCP-based UI inspection server for debugging GExperts forms (disabled by default, enable with --enable-gxinspect build flag)
- GxInspect: Added action commands: EXPERTS, EXECUTE, CONFIGURE, SETBOUNDS, CLOSE, CLICK, GETPROP, SETPROP, QUIT
- GxInspect: Added OPENFILE, SHOWDESIGNER and DESIGNSCREENSHOT commands for form designer screenshots
- GxInspect: Added -t timeout option with retry logic for waiting until the server becomes available
- GxInspect: Added GXCONFIG command to open the main GExperts configuration dialog
- GxInspect: GETPROP and SETPROP now support dotted property paths (e.g. Font.Size, Font.Name)
- GxInspect: Control name now supports [Index] syntax for accessing unnamed children (e.g. rg_FilterIdentifiers[0] for TGroupButton inside TRadioGroup)
- Configuration: Added option to disable IDE theming for GExperts forms (Delphi 11+, requires IDE restart)
- Class Browser: Added toolbar toggle to show/hide implementation-section classes, with greyed icons to distinguish them from interface-section classes (bug #24)
- Rename Identifier: New scope-aware editor expert (Ctrl+Alt+R) that renames identifiers within a single unit, supporting local, class, and unit-level scopes with shadowing protection (feature #182)
- Rename Components: Shift+F2 now forwards to Rename Identifier when the code editor is focused, so the same shortcut works in both the form designer and code editor
- Rename Identifier: Renaming a component field in source now also renames the component in the form designer (with warning label in dialog)
- Rename Identifier: Dialog now shows identifier kind (variable, constant, type, parameter, field, property, method), declared type, and scope with interface/implementation distinction (feature #182)
- Rename Identifier: Conflict detection warns when the new name clashes with a visible identifier (200ms debounce, informational only) (feature #182)
- Rename Components: After renaming a component, source code references are also renamed (optional, controlled by new checkbox in config)
- Declare Variable: New editor expert that declares an undeclared identifier as a local variable, class field, or property with type inference and configurable insertion point
- Declare Variable: Type inference from literals, booleans, nil, constructors, existing variable types, function return types, and parameter types of locally declared functions/methods
- Declare Variable: Type inference from compiler intrinsic functions (Length, High, Low, Ord, Chr, Copy, Pos, Assigned, etc.)
- Declare Variable: Type inference from external units via Uses Clause Manager — resolves function return types, parameter types, enum value types, and typed constant types from units in the current uses clause
- Declare Variable: Type inference for overloaded intrinsics (Abs, Sqr, Pred, Succ) based on argument type analysis, with alternative type suggestions shown in a dropdown
- Declare Variable: Type field changed to combobox with dropdown for selecting alternative types when multiple are possible (e.g. Integer/Cardinal/Int64/UInt64/Extended for Abs)
- Declare Variable: External overloaded functions now collect all distinct return types as alternatives
- Declare Variable: Added accelerator hotkeys (Alt+T, Alt+I, Alt+R, Alt+W) for Type, Insert at, Read specifier, and Write specifier fields
- Declare Variable: Type inference now resolves inherited methods — covers `inherited Method`, `Self.Method`, `Obj.Method` on ancestors, and unqualified method calls inside class method bodies (e.g. `s := ClassName` resolving to `ShortString` via TObject)

## Bug Fixes

- Declare Variable: Field declarations no longer add an F prefix — the identifier is used as-is
- Declare Variable: Fixed field insertion landing after method declarations — function return types and method parameters were falsely detected as fields
- Declare Variable: All visibility sections (private, strict private, protected, strict protected, public, published) are now offered for field/property insertion in fixed order, with the option to create new sections
- Declare Variable: Default insertion point for fields and properties is now the private section
- Declare Variable: Cursor now stays on the identifier after inserting a declaration above the current line

- Declare Variable: Fixed existing-variable warning label overlapping controls below when the message wraps to multiple lines
- Uses Clause Manager: Fixed anonymous enum values in `set of (...)` declarations not being indexed in the UCM cache (bug #478)
- Rename Identifier: Fixed renaming method parameters not propagating between class declaration and implementation (feature #182)
- Rename Identifier: Result is now correctly scoped to its enclosing function instead of being treated as a unit-level identifier; nested procedures refer to the enclosing function's Result, nested functions shadow it (feature #182)
- Rename Components: Shift+F2 only forwards to Rename Identifier when Rename Identifier has no shortcut of its own
- Rename Identifier: Fixed identifier not detected when cursor is right after it (e.g. after selecting text or on semicolon/paren following an identifier)

- Uses Clause Manager: Line wrapping now respects IDE Right Margin setting instead of hardcoded 80 columns

- PasteAs / Convert Raw Strings: Renamed configuration dialog to "String Conversion Options" and added a note that these settings are shared between Paste Strings As, Convert Raw Strings and Clipboard History experts (bug #362)
- PasteAs / Convert Raw Strings: Fixed "List index out of bounds (0)" error when selected lines contain no quoted strings
- ToDo List / Bookmarks: Fixed non-ASCII ANSI characters (e.g. Chinese, Polish) being corrupted in Delphi 6/7 by skipping the unnecessary UTF-8 conversion for pre-Delphi 8 builds (bug #176)
- Reverse Statement: Fixed regex matching bare-dot expressions like `.ReadInteger()` which produced invalid code (feature #14)
- Backup Project: Show actual matched files instead of wildcard patterns in file list (bug #19)
- Backup Project: Added ListView with Path, File Name, Size, and Last Modified columns
- Backup Project: Added {#+BACKUP} directive for recursive folder search
- Backup Project: Added {#-BACKUP} directive for exclude patterns
- Code Proofreader: AutoCorrect now triggers when pressing Enter after typing a word (bug #21)
- Code Formatter: "Never indent var declaration" no longer affects "and"/"or" in if conditions (bug #468)
- Code Formatter: Generic detection now handles dot-qualified type names like `System.TJSONObject` (bug #471)
- Instant Grep: Fixed wrong colors when form is restored by IDE docked desktop (bug #470)
- DPI Scaling: bsDialog forms now correctly scale their size at non-100% DPI (was disabled for all forms since 2024-05-18)
- DPI Scaling: Fixed form font not scaling back down when moving a form from a higher-DPI monitor to a lower-DPI monitor, without reintroducing bug #439 (double-scaled fonts when all monitors use the same >100% scaling)
- Uses Clause Manager: Replaced TRadioGroup with TGroupBox + TRadioButton in configuration dialog
- Uses Clause Manager: Fixed radio button spacing in configuration dialog and improved DPI-aware layout (anchoring, ArrangeControls, Help button anchoring)
- Rename Components: Improved DFM layout to prevent button clipping at non-100% DPI
- Edit Search Path: Dialog no longer grows on each open with mixed-DPI monitor setups (bug #472)
- Replace Components: Replacing components with sub-components now works again in Delphi 13 (Embarcadero fixed the underlying OTA bug)
- Code Formatter: Fixed wrong indentation of continuation lines in multiline repeat-until conditions (bug #473)
- Code Formatter: Fixed wrong indentation of begin/end in anonymous functions/procedures after := assignment (wronly filed as feature request #165)
- Procedure List: Fixed `(*` inside Delphi 12+ triple-quoted multi-line strings being misinterpreted as ANSI comment start, causing methods below the string to disappear (bug #475)
- Code Formatter: Fixed missing space before `at` in `raise ... at P` statements (bug #476)
- Code Formatter: Fixed missing indent for continuation lines in mid-line multiline block comments when IndentComments is active (bug #367)
- Expert Manager: Fixed duplicate entries and wrong DLL path shown when the same expert name exists in both enabled and disabled registries (bug #447)
- Procedure List: Anonymous methods (procedure/function without a name) no longer create bogus entries and no longer break the body range of the enclosing procedure (bug #63)
- Class Browser: Fixed inheritance panel hint showing form title instead of class name
- Class Browser: Fixed ClassHierarchyFont setting always saving as 'Arial' instead of the user's chosen font
- Class Browser: Fixed SetInfoViewMode clearing treeview selection even when mode didn't change
- Class Browser: Fixed potential crash when parsing empty source files (nil dereference on empty FileContent)
- Class Browser: Fixed potential crash from dangling pointers when removing projects or files (OnChange firing during node deletion)
- Class Browser: Added nil check for empty treeview in GetInheritedList
- Class Browser: Removed Application.ProcessMessages from print loop to prevent re-entrant UI interactions
- Class Browser: Added circular inheritance detection to prevent infinite loop from corrupt cache files
- Class Browser: Fixed line number always 0 for generic class/interface declarations like `TList<T>` or `TFoo<T: IBar>` (bug #36)
- Class Browser: Fixed TClassBrowswerFilters typo (renamed to TClassBrowserFilters)
- Class Browser: Fixed nested type declarations (class/record inside class) having their members attributed to the outer class instead of being skipped (bug #248)
- Class Browser: Nested classes now show with fully qualified names (e.g. TOuterClass.TInnerClass) in tree and list views (bug #248)
- Class Browser: Fixed Print Class Hierarchy printing duplicate nodes for deep inheritance trees (bug #355)
- Source Export: Replaced 16-color grid with full color dialog for highlighting colors (bug #119)
- Uses Clause Manager: Fixed nested `$IFNDEF`/`$ELSE` not being handled correctly by the unit exports parser — `SkipToEndif` now recurses into nested `$IFNDEF` blocks, matching the existing `$IFDEF` handling (bug #232)
- Unit Tests: Fixed `_BuildProject.cmd` failure paths in all unit test projects (including UnitTestsFormatter) using `exit /b 1` instead of `goto :done` so build failures are correctly propagated to the bash wrapper
- Unit Tests: Fixed I/O error 105 in TestDeclareVariable and TestRenameIdentifier console mode by calling `AttachConsole`/`AllocConsole` before any `WriteLn`

## Contributors

- Thomas Mueller (twm)
- freeman35
