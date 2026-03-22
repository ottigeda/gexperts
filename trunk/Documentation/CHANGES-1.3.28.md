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

## Bug Fixes

- Code Formatter: Nested generic types like `TJSON.JsonToObject<TJSONReport<TPrintData>>` are now recognized correctly instead of being treated as comparison operators (bug #219)
- Open File expert: Dialog position and size are now saved even when the dialog is cancelled (bug #480)

## Contributors

- Thomas Mueller (twm)
