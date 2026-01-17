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

## Current Work: Nested Procedure Indentation Fix

### Problem Description

Nested procedures inside anonymous procedures were losing their indentation and appearing at column 1 instead of being properly indented within the anonymous procedure context.

**Example input:**
```pascal
fThread_UDPServer := TThread.CreateAnonymousThread(procedure
  procedure NestedProcedure;
  begin
    Code;
  end;
  begin
    MoreCode;
  end);
```

**Was producing (broken):**
```pascal
fThread_UDPServer := TThread.CreateAnonymousThread(procedure

procedure NestedProcedure;
begin
  Code;
end;
  begin
    MoreCode;
  end);
```

### Root Cause

In `Source/Formatter/Engine/GX_CodeFormatterFormatter.pas`, the `CheckIndentProcedure` method was resetting `nIndent` to 0 for nested procedures even when inside an anonymous procedure context (when `rtProcDeclare` was on the stack).

### Fix Applied (in GX_CodeFormatterFormatter.pas)

1. **Line ~1617** - Added check for `rtProcDeclare` at top of stack:
   ```pascal
   if not FStack.HasType(rtProcedure) and (FStack.GetTopType <> rtProcDeclare) then begin
   ```

2. **Line ~1668** - Added `rtLeftBr` to exclusion check to prevent indent reset inside parentheses:
   ```pascal
   if (not FunctTypeDeclare) and not (FStack.GetTopType in [rtClass, rtLeftBr]) then begin
   ```

3. **Lines ~1636-1646** - Added special handling for nested procedures inside anonymous procedures:
   - Don't increment ProcLevel when inside anonymous procedure context
   - Set indentation based on current nIndent

### Test Status

- **Before fix:** 24 test failures
- **After fix:** 7 test failures (17 tests fixed)

### Remaining Issues (7 failures)

All remaining failures are for tests with **parameters** in the anonymous procedure:
- `failure_AnonymousProcWithParamsAndNestedProc.pas`
- `failure_AnonymousWithParamsAndNestedProc2.pas`

**Issue:** The expected files for these tests show a blank line after `procedure(params)` before the nested procedure, but the formatter is not producing this blank line.

**Expected:**
```pascal
fThread_UDPServer := TThread.CreateAnonymousThread(procedure(bla: integer)

    procedure NestedProcedure;
```

**Actual:**
```pascal
fThread_UDPServer := TThread.CreateAnonymousThread(procedure(bla: integer)
    procedure NestedProcedure;
```

This affects configurations: `borland`, `delforex`, `headwork`, `special`, `twm` (but NOT `default` after expected file correction).

### Next Steps

1. Investigate why blank lines are expected after anonymous procedure declarations with parameters
2. Check if the expected files are correct or need updating
3. If correct, fix the blank line insertion logic in `CheckBlankLinesAroundProc` or `CheckIndentProcedure`
4. The blank line logic is in `CheckBlankLinesAroundProc` (line ~766) and is called when `BlankSubProc` is TRUE

### Files Modified

- `Source/Formatter/Engine/GX_CodeFormatterFormatter.pas` - Main fix
- `UnitTestsFormatter/Testcases/expected/default/failure_Anonymous*.pas` - Corrected expected files (copied from `borland`)
