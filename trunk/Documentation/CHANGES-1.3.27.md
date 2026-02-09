# GExperts 1.3.27 Changes

Changes since version 1.3.26 (released 2026-02-07)

## New Features

## Bug Fixes

- Backup Project: Show actual matched files instead of wildcard patterns in file list (bug #19)
- Backup Project: Added ListView with Path, File Name, Size, and Last Modified columns
- Backup Project: Added {#+BACKUP} directive for recursive folder search
- Backup Project: Added {#-BACKUP} directive for exclude patterns
- Code Proofreader: AutoCorrect now triggers when pressing Enter after typing a word (bug #21)
- Code Formatter: "Never indent var declaration" no longer affects "and"/"or" in if conditions (bug #468)
- Code Formatter: Generic detection now handles dot-qualified type names like `System.TJSONObject` (bug #471)
- Instant Grep: Fixed wrong colors when form is restored by IDE docked desktop (bug #470)
- Replace Components: Replacing components with sub-components now works again in Delphi 13 (Embarcadero fixed the underlying OTA bug)

## Contributors

- Thomas Mueller (twm)
- freeman35
