# GExperts 1.3.28 Changes

Changes since version 1.3.27 (released 2026-03-21)

## New Features

- Grep: Access error messages (missing directories, inaccessible files) now use a dialog with a Details button to hide the potentially long error list, and a "Don't show again" checkbox to suppress the dialog permanently (feature #221)
- Message box framework: `TGxMsgBoxAdaptor` now supports a `GetDetails` method to show detail text behind a Details button
- Comment Code expert: When the configured block comment style (`{ }`, `(* *)`, or `/* */`) would produce broken nested comments, the expert now offers to fall back to `//` line comments instead. The "Don't show again" checkbox remembers the choice. The Uncomment expert auto-detects `//`-commented blocks when a block style is configured (feature #178)
- Message box framework: `DoPermanentlySuppress` and `IsPermanentlySuppressed` are now virtual, enabling per-result suppression in subclasses

## Bug Fixes

- Code Formatter: Nested generic types like `TJSON.JsonToObject<TJSONReport<TPrintData>>` are now recognized correctly instead of being treated as comparison operators (bug #219)

## Contributors

- Thomas Mueller (twm)
