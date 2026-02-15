# GExperts 1.3.27 Changes

Changes since version 1.3.26 (released 2026-02-07)

## New Features

- Code Formatter: Added option to use identifiers from Uses Clause Manager for capitalization (feature #218)
- Code Proofreader: Added option to use identifiers from Uses Clause Manager for case correction (feature #218)
- Code Formatter: Added option to align assignment operators (:=) at a fixed column position
- Reverse Statement: Replaced simple function name pairs with configurable regex-based reversal rules (feature #14)
- Reverse Statement: Default rules now correctly handle Read/Write method transformations (e.g. ReadString/WriteString)
- Reverse Statement: Added configuration dialog with interactive Test button
- Reverse Statement: Redesigned configuration dialog with two-column grid (match/replacement) replacing raw text editor (feature #14)
- Reverse Statement: Added edit sub-dialog for adding/editing individual reversal rules with real-time regex validation and test (feature #14)
- Reverse Statement: Moved source files to dedicated Source/ReverseStatement/ directory (feature #14)

## Bug Fixes

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
- Rename Components: Improved DFM layout to prevent button clipping at non-100% DPI
- Edit Search Path: Dialog no longer grows on each open with mixed-DPI monitor setups (bug #472)
- Replace Components: Replacing components with sub-components now works again in Delphi 13 (Embarcadero fixed the underlying OTA bug)
- Code Formatter: Fixed wrong indentation of continuation lines in multiline repeat-until conditions (bug #473)
- Code Formatter: Fixed wrong indentation of begin/end in anonymous functions/procedures after := assignment (wronly filed as feature request #165)

## Contributors

- Thomas Mueller (twm)
- freeman35
