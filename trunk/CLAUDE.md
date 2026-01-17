# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

GExperts is an open-source IDE plugin (DLL) for Delphi and C++Builder that provides productivity tools and code enhancements. It supports Delphi 6 through RAD Studio 13.0 (including 64-bit).

## Build Commands

**Build a specific Delphi version:**
```cmd
cd Projects\Delphi2007
__Build_Project.cmd
```

**Build all versions:**
```cmd
__Build.cmd
```

**Clean build artifacts:**
```cmd
__clean.cmd
```

The build system uses MSBuild for Delphi 2007+ and DCC32 directly for older versions. Delphi installation paths are auto-detected from Windows Registry.

**Output location:** `Binaries/GExpertsRS###.dll` where ### is the Delphi version number.

## Testing

**Code Formatter tests (GUI):**
```cmd
cd UnitTestsFormatter
GX_FormatterUnitTests.exe
```

**Code Formatter tests (Console for CI/automation):**
```cmd
cd UnitTestsFormatter
GX_FormatterUnitTests.exe --console
GX_FormatterUnitTests.exe --console results.txt
```

Console runner options:
- `-c` or `--console` - Run in console mode with output to console
- `-c <file>` or `--console <file>` - Run in console mode with output to file

Exit code equals the number of test errors + failures, suitable for CI systems.

**Build formatter tests (Delphi 13):**
```cmd
cd UnitTestsFormatter\source
call c:\Delphi\13\bin\rsvars.bat && msbuild /target:rebuild /p:config=Release GX_FormatterUnitTests.13.dproj
```

**Build formatter tests (WSL/bash):**
```bash
cd UnitTestsFormatter
bash _BuildProject.sh
```

**Run formatter tests (WSL/bash):**
```bash
cd UnitTestsFormatter
cmd.exe /c "GX_FormatterUnitTests.exe --console"
```

**Unit Exports Parser tests:**
```cmd
cd UnitTests\TestUnitExportsParser\src
# Open and run TestUnitExportsParser.dpr in Delphi IDE
```

### Formatter Test Case Structure

Test cases are located in `UnitTestsFormatter/Testcases/`:

| Directory | Purpose |
|-----------|---------|
| `input/` | Input Pascal files to be formatted |
| `expected/<config>/` | Expected output for each formatter configuration |
| `output/<config>/` | Actual output (only written when tests fail) |

Formatter configurations: `borland`, `default`, `delforex`, `headwork`, `special`, `twm`

**Naming conventions:**
- `testfile_*.pas` - Regular test files
- `failure_*.pas` - Known failing tests that need to be fixed

When a test fails, the incorrectly formatted output is saved to `output/<config>/` for comparison with `expected/<config>/`. Use Beyond Compare or similar diff tool to analyze differences.

### Formatter Engine

The core formatting engine is in `Source/Formatter/Engine/GX_CodeFormatterFormatter.pas`. The main class `TCodeFormatterFormatter` generates formatted Pascal code from a token collection.

### Verifying Formatter Changes

After modifying the formatter source code:

1. **Rebuild the test runner:**
```cmd
cd UnitTestsFormatter
_BuildProject.cmd
```

2. **Run the tests:**
```cmd
cd UnitTestsFormatter
GX_FormatterUnitTests.exe --console
```

**Important:** Always verify that:
1. The failing tests you're trying to fix now pass
2. All previously passing tests still pass (no regressions)

## Architecture

### Expert Plugin Pattern

All features are implemented as "Experts" deriving from a base class hierarchy:

```
TGX_BaseExpert (Source/Framework/GX_BaseExpert.pas)
├── TGX_Expert - IDE menu/toolbar experts
└── TEditorExpert - Editor enhancement experts
```

Each expert implements:
- `GetName()` / `GetDisplayName()` - Identification
- `Execute()` - Main functionality
- `Configure()` - Configuration dialog
- `CanHaveShortCut()` - Keyboard binding support

### Key Framework Files

| File | Purpose |
|------|---------|
| `Source/Framework/GX_BaseExpert.pas` | Abstract expert base class |
| `Source/Framework/GX_Experts.pas` | Expert manager and registration |
| `Source/Framework/GX_Configure.pas` | Configuration UI framework |
| `Source/Framework/GX_Actions.pas` | Action/menu integration |
| `Source/Framework/GX_OtaUtils.pas` | OpenTools API wrapper utilities |

### Multi-Version Support

Each Delphi version has its own project folder under `Projects/`:
- `Projects/Delphi6/`, `Projects/Delphi7/`
- `Projects/Delphi2005-2007/` (BDS 3.0-5.0)
- `Projects/DelphiXE1-XE8/`
- `Projects/DelphiXx10Seattle/` through `Projects/DelphiXx130/`
- `Projects/DelphiXx130_64/` (64-bit)

All versions share the same source code in `Source/` with conditional compilation.

### Expert Categories

| Directory | Purpose |
|-----------|---------|
| `Source/Editor/` | Code editing (align, sort, comment, paste-as) |
| `Source/Formatter/` | Code formatting with parser engine |
| `Source/Grep/` | Search and replace across files |
| `Source/ClassBrowser/` | OOP structure viewer |
| `Source/CodeLibrarian/` | Code snippet library |
| `Source/Bookmarks/` | Code bookmarks |
| `Source/ToDoList/` | TODO item tracking |
| `Source/IDE/` | IDE enhancement modules |

### External Dependencies

Located in `ExternalSource/`:
- **Abbrevia** - ZIP/archive compression
- **RegExpr** - Regular expression engine
- **OmniXML** - XML parsing
- **UniSynEdit** - Unicode syntax highlighting
- **mwParser** - Pascal code parsing

### Standalone Utilities

Separate executables sharing the codebase in `Projects/StandAlone/`:
- CodeFormatter, CodeLibrarian, DebugWindow, ExpertManager, Grep, PeInformation

## Configuration

- Settings stored in Windows Registry under `Software\GExperts\`
- Version info in `Projects/GExperts_version.ini`
- SVN revision auto-captured in `SVN_Version.ini` during build

## IDE Registration

GExperts registers with the Delphi IDE via:
```
HKEY_CURRENT_USER\Software\Embarcadero\BDS\<version>\Experts
GExperts = <path to DLL>
```

## Delphi Version Mapping

| Delphi Version | BDS Version | Project Folder |
|----------------|-------------|----------------|
| Delphi 6 | 6.0 | Delphi6 |
| Delphi 7 | 7.0 | Delphi7 |
| Delphi 2007 | 5.0 | Delphi2005-2007 |
| Delphi XE8 | 16.0 | DelphiXE1-XE8 |
| RAD Studio 10 Seattle | 17.0 | DelphiXx10Seattle |
| RAD Studio 11 Alexandria | 22.0 | DelphiXx110Alexandria |
| RAD Studio 12 | 23.0 | DelphiXx120 |
| RAD Studio 13 | 37.0 | DelphiXx130 |

## Formatter Configuration System

Formatter settings are loaded from INI files in `Binaries/`:
- `FormatterSettings-Borland.ini`
- `FormatterSettings-DelForEx.ini`
- `FormatterSettings-headwork.ini`
- `FormatterSettings-twm.ini`

The `default` configuration does NOT have an INI file - it uses `BorlandDefaults` from `Source/Formatter/GX_CodeFormatterDefaultSettings.pas`, initialized in `TCodeFormatterSettings.Create` in `Source/Formatter/Engine/GX_CodeFormatterSettings.pas`.

Key settings affecting indentation:
- `IndentBegin` - Whether `begin` adds extra indentation
- `BlankSubProc` - Whether to add blank lines around sub-procedures
- `BlankProc` - Whether to add blank lines around procedures

## Recent Fix: Nested Procedure Indentation in Anonymous Procedures

Fixed in `Source/Formatter/Engine/GX_CodeFormatterFormatter.pas`:

1. **Nested procedure indentation** - Nested procedures inside anonymous procedures now maintain proper indentation instead of resetting to column 1.

2. **Blank line after anonymous procedure with parameters** - Added `AnonymousProcHadParams` function to detect when an anonymous procedure has parameters and insert a blank line before nested content.
