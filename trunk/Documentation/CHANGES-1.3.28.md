# GExperts 1.3.28 Changes

Changes since version 1.3.27 (released 2026-03-21)

## New Features

- Grep: Access error messages (missing directories, inaccessible files) now use a dialog with a Details button to hide the potentially long error list, and a "Don't show again" checkbox to suppress the dialog permanently (feature #221)
- Message box framework: `TGxMsgBoxAdaptor` now supports a `GetDetails` method to show detail text behind a Details button
- Comment Code expert: When the configured block comment style (`{ }`, `(* *)`, or `/* */`) would produce broken nested comments, the expert now offers to fall back to `//` line comments instead. The "Don't show again" checkbox remembers the choice. The Uncomment expert auto-detects `//`-commented blocks when a block style is configured (feature #178)
- Message box framework: `DoPermanentlySuppress` and `IsPermanentlySuppressed` are now virtual, enabling per-result suppression in subclasses
- Open File expert: New "Project Group" tab shows files from all projects in the project group. The tab is hidden when only a single project is loaded. The Project tab's content has been extracted into a reusable frame (feature #175)
- IDE Form Enhancer: The debugger's Exception dialog (and other IDE message dialogs with only an OK button) can now be closed with Esc (feature #206)
- Configuration: New "Disable IDE license notification" checkbox on the IDE tab writes the `LicenseNotificationEnabled` registry value to suppress the IDE's license notification popup (feature #202)
- Vertical Tabs: New dockable panel showing open editor files as a vertical list. Single-click to switch files. Supports alphabetical or tab-order sorting, and a context menu with Close File, Close All Other, Copy Path, and Open Containing Folder (feature #224)

## Enhancements

- Grep: The match context preview now uses a syntax-highlighted code editor (SynEdit) with IDE editor colors instead of a plain RichEdit. Highlighting adapts automatically to the file type (Pascal, C++, HTML, SQL, XML). Match highlighting (line background + matched text color/bold) is preserved on top of syntax colors.
- Grep: Context match colors configuration now uses a "Use default colors" checkbox instead of a Reset button, matching the pattern used for list colors.
- Grep: Context preview syntax colors now update when switching between light and dark mode (feature #222)

## Bug Fixes

- Code Formatter: Nested generic types like `TJSON.JsonToObject<TJSONReport<TPrintData>>` are now recognized correctly instead of being treated as comparison operators (bug #219)
- Open File expert: Dialog position and size are now saved even when the dialog is cancelled (bug #480)
- Grep: Work around blank docked Grep Results window by calling ForceRedraw after ShowForm
- Auto Switch Desktop: Fixed crash in Debug builds when pressing F12 — `GetAsyncKeyState` with `SmallInt` caused a range check error; replaced with `GetKeyState` and `< 0` test (bug #481)
- Vertical Tabs: Fixed file switching not working in Delphi 6/7 by pausing the polling timer during switch. Project and project group files are now filtered from the list (feature #226)

## Contributors

- Thomas Mueller (twm)
