# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

GExperts is an open-source IDE plugin (DLL) for Delphi and C++Builder that provides productivity tools and code enhancements. It supports Delphi 6 through RAD Studio 13.0 (including 64-bit).

## Version Control

**IMPORTANT:** This project uses Subversion (SVN), not Git.

**Server operations (require Windows svn.exe in WSL):**
```cmd
svn.exe commit -m "Commit message" <files>
svn.exe update
svn.exe list <path>     # List repository contents
svn.exe log <file>      # Show commit history from repository
```

**Local operations (work with standard svn):**
```cmd
svn status          # Check modified files
svn diff <file>     # View changes
svn add <file>      # Add new file to version control
svn info <file>     # Show local file info
```

**Note:** When working in WSL, server operations (commit, update, list, log, etc.) must use the Windows `svn.exe` executable due to connectivity issues with the SourceForge repository. Local operations (status, diff, add, info, etc.) work fine with the Linux SVN client.

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
bash _buildproject.sh
```

**Run formatter tests (WSL/bash):**
```bash
cd UnitTestsFormatter
bash _runformatterunittests.sh
```

Results are written to `UnitTestsFormatter/testresults.txt`.

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

### Workflow for Fixing Formatter Bugs

When fixing a formatter bug:

1. **Create a new test input file** in `UnitTestsFormatter/Testcases/input/` (e.g., `testfile_MyBug.pas`)
2. **Run the tests** to see the current (buggy) formatter output
3. **Show the actual formatter output** to the user and **ask for confirmation** before updating any expected files
4. **Only after approval**, update the expected files for the new test
5. **If existing tests are affected** by the fix, show the differences and **ask for confirmation** before updating those expected files
6. Run all tests to verify no regressions

**Important:** Never modify existing expected test files without explicit user approval. The user needs to verify that the new formatter behavior is correct before test expectations are changed.

**File encoding warning:** Some test files (e.g., `testfile_LargeFile.pas`) use ANSI encoding with special characters (German umlauts, etc.). Using `sed` or other Unix tools in WSL may convert them to UTF-8 and corrupt these characters. Prefer using the Edit tool for modifications to preserve encoding.

### Workflow for Adding New Formatter Options

When adding a new formatter option (e.g., a new boolean setting):

**CRITICAL: Create test cases FIRST, before changing any code!**

1. **Create test input file** in `UnitTestsFormatter/Testcases/input/` (e.g., `testfile_MyNewFeature.pas`)
2. **Run tests with the ORIGINAL formatter** to generate baseline expected files:
   ```cmd
   cd UnitTestsFormatter
   GX_FormatterUnitTests.exe --console
   ```
3. **Copy the output files to expected directories** for all configurations where the new feature will be disabled by default:
   ```cmd
   for config in borland default delforex headwork twm; do
     cp Testcases/output/$config/testfile_MyNewFeature.pas Testcases/expected/$config/
   done
   ```
4. **Now implement the formatter changes** (settings, UI, formatter logic)
5. **Enable the new setting** in `AdjustSettings_Special` in `UnitTestsFormatter/source/DelForExTestFiles.pas`
6. **Rebuild and run tests** - the new test will fail for the `special` config (expected)
7. **Copy the special config output** to expected:
   ```cmd
   cp Testcases/output/special/testfile_MyNewFeature.pas Testcases/expected/special/
   ```
8. **Update any existing tests affected** by the new feature (check `output/special/` for other files)
9. **Run final test verification** to ensure all 1324+ tests pass

**Why create tests first?** This workflow ensures you capture the correct baseline behavior using the unmodified formatter. If you change the code first, you'll need to manually recreate what the original output should have been, which is error-prone.

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
- `BlankBeforeUses` - Whether to add a blank line before `uses` clauses (enabled in `twm` and `special` configs)
- `IndentCompDirectives` - How compiler directives are indented (see below)

### Compiler Directive Indentation (IndentCompDirectives)

The `IndentCompDirectives` setting is a `TIndentCompDirectives` enum with three values:
- `icdNone` (=0) - Place directives at column 1 (default)
- `icdFollowCode` (=1) - Follow surrounding code indentation
- `icdIndependent` (=2) - Indent by directive nesting level, independent of code

**INI backward compatibility:** Old boolean values map correctly (0=icdNone, 1=icdFollowCode).

**Implementation notes** (in `GX_CodeFormatterFormatter.pas`):
- `FCompDirectiveNestLevel` tracks the current directive nesting depth
- Nesting level must be tracked for ALL directives, including inline ones like `{$ELSE}Windows, {$ENDIF}` where `{$ENDIF}` is not at the start of a line
- `FStack.nIndent` is in indent **levels** (0, 1, 2...), not spaces. The `SetIndent` method multiplies by `SpacePerIndent`
- For `rtCompEndif` indentation, use `FCompDirectiveNestLevel - 1` to match the opening `{$IFDEF}`

## Recent Fixes

Fixed in `Source/Formatter/Engine/GX_CodeFormatterFormatter.pas`:

1. **Continuation indent after single-line comments** - Continuation indentation is now preserved through sequences of `//` comments in expressions. Previously, `FWrapIndent` was reset for all comments, breaking continuation. The fix only resets `FWrapIndent` for block comments (`{ }`, `(* *)`) or when in case statements (`rtOf` context).

2. **Nested procedure indentation** - Nested procedures inside anonymous procedures now maintain proper indentation instead of resetting to column 1.

3. **Blank line after anonymous procedure with parameters** - Added `AnonymousProcHadParams` function to detect when an anonymous procedure has parameters and insert a blank line before nested content.

4. **Independent compiler directive indentation** - Added `icdIndependent` mode that indents `{$IFDEF}`, `{$IF}`, `{$ELSE}`, `{$ENDIF}`, `{$IFEND}` based on their own nesting level, independent of surrounding code. Inline directives (not at start of line) are tracked for nesting but not indented.

5. **Blank line before uses clauses** - Added `BlankBeforeUses` setting to insert a blank line before `uses` clauses. When comments immediately precede a uses clause, the blank line is inserted before the comments to keep them visually grouped with the uses. Enabled by default in `twm` and `special` configurations.

6. **Nested procedure indentation with BlankSubProc** - Fixed bug where nested procedures were not indented when there was no blank line above them in the input. The formatter now sets indentation before inserting blank lines (previously the blank line insertion changed `FPrevLine`, causing indentation to be applied to the wrong line).

## Delphi 13 Ternary Operator Support

Delphi 13 (RAD Studio 13 Florence) introduced inline if expressions, also called the "ternary operator" or "conditional operator". This feature allows `if-then-else` to be used as an expression that returns a value.

**Syntax**: `if <condition> then <trueValue> else <falseValue>`

**Key characteristics**:
- Short-circuit evaluation (only evaluates the selected branch)
- The `else` part is mandatory (cannot be omitted)
- Can be used anywhere an expression is valid
- Can be nested (parentheses recommended for clarity)
- Can create compile-time constants when all parts are constants

**Formatter implementation**: The formatter keeps inline if expressions on one line with proper spacing:

```pascal
// Simple ternary
X := if Y > 0 then 10 else 20;

// Nested ternary
X := if Y > 0 then (if Y > 10 then 100 else 50) else 0;

// In constants
const MAX_SIZE = if True then 100 else 50;

// In case statement
case if Y > 0 then Y else -Y of
  1: X := 1;
end;

// In for loop
for X := 1 to if Y > 0 then Y else 10 do
  WriteLn(X);
```

**Test case**: `UnitTestsFormatter/Testcases/input/testfile_TernaryOperator.pas` contains comprehensive test cases.

**Implementation details** in `Source/Formatter/Engine/GX_CodeFormatterFormatter.pas`:
- `FInlineIfDepth` field tracks ternary operator nesting depth
- `IsInlineIfExpression()` detects if expression context based on previous token
- `CheckIndentIf`, `CheckIndentThen`, `CheckIndentElse` skip line-breaking for ternary operators
- Special handling in `rtOf` and `rtDo` to clean up stack markers for proper indentation

**Future enhancement**: Add configuration option for vertical alignment style (Phase 2).

**References**:
- [Embarcadero Blog: Coming in RAD Studio 13](https://blogs.embarcadero.com/coming-in-rad-studio-13-a-conditional-ternary-operator-for-the-delphi-language/)
- [Intentional Delphi: The Conditional Ternary Operator](https://wmeyer.tech/2025/10/20/the-conditional-ternary-operator/)
- [Delphi-PRAXiS Discussion](https://en.delphipraxis.net/topic/13907-a-conditional-ternary-operator-for-the-delphi/)
