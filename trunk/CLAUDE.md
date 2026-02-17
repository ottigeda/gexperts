# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

GExperts is an open-source IDE plugin (DLL) for Delphi and C++Builder that provides productivity tools and code enhancements. It supports Delphi 6 through RAD Studio 13.0 (including 64-bit).

## Development Approach

- **Investigate before implementing.** Before making code changes, read the relevant source files, trace the code path, and understand the existing patterns. For bug fixes, identify the root cause before proposing a fix. When a fix introduces regressions, step back and analyze rather than trying another quick approach.
- **Run build and test commands separately.** Do NOT chain them in a single Bash call (e.g., don't use `build.sh && test.sh`). Run each as a separate tool invocation to avoid timeouts and unclear failures.
- **Use existing build infrastructure.** Always call `__Build_Project.cmd` or the documented bash wrapper scripts (`_buildproject.sh`). Do NOT write custom build logic from scratch.
- **Clean up temporary files.** Do not leave debug or temporary test files in the working directory. If temp files are needed during a task, clean them up before finishing.

## DFM Compatibility

**IMPORTANT:** Never use `Margins` or `AlignWithMargins` properties in DFM files. These were introduced in Delphi 2006 and cause errors in Delphi 6/7. The Delphi form designer auto-adds these properties, so always check DFM files before committing.

**Before committing any DFM file**, always run the DfmFilter tool to check for offending properties:
```bash
./Binaries/DfmFilter.exe <dfm-file>
```
If it reports problems, fix them with `--fix` or manually before committing.

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

**Commit message guidelines:**
- Do NOT include a list of changed files in commit messages - SVN tracks this automatically
- Do NOT include a Co-Authored-By line in commit messages
- Focus on describing what was changed and why

**After committing:** Always update the changelog file `Documentation/CHANGES-<version>.md` (currently `CHANGES-1.3.27.md`) with a brief description of the change.

## Build Commands

**Build a specific Delphi version:**
```cmd
cd Projects\Delphi2007
__Build_Project.cmd
```

**Build from WSL:**
```bash
cd Projects/DelphiXx130
bash _buildproject.sh
```

Each project directory has a `_buildproject.sh` script that calls `__Build_Project.cmd --nowait`.

**Build all versions:**
```cmd
__Build.cmd
```

**Clean build artifacts:**
```cmd
__clean.cmd
```

The build system uses MSBuild for Delphi 2007+ and DCC32 directly for older versions. Delphi installation paths are auto-detected from Windows Registry.

**Output location:** `Binaries/GExpertsRS###.dll` where ### is the Delphi version number (64-bit: `GExperts64RS###.dll`).

## Installer Build

Installers are built using Inno Setup scripts in `BuildInstall/`. See `BuildInstall/CLAUDE.md` for detailed documentation.

**Build all installers:**
```bash
cd BuildInstall
bash _makeinstallers.sh
```

The RS13 installer includes both 32-bit and 64-bit DLLs, with an optional checkbox to register for the 64-bit IDE.

## Testing

**Build formatter tests:**
```bash
cd UnitTestsFormatter
bash _buildproject.sh
```

**Run formatter tests:**
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
- `testfile_*.pas` - Regular test files (tests pass when formatter produces expected output)
- `failure_*.pas` - Known bug documentation files (tests FAIL until formatter is fixed)

**Important:** `failure_*.pas` expected files contain CORRECT formatting (what the formatter SHOULD produce), not the current buggy output. These tests intentionally fail to document bugs. When someone fixes a formatter bug, the corresponding failure tests will start passing.

**Auto-creation of expected files:** If no expected file exists for a test input, the framework automatically copies the input file to `expected/<config>/` and uses that as the baseline. The formatted output is then compared against this copy. This means tests always run regardless of whether expected files were pre-created.

When a test fails, the incorrectly formatted output is saved to `output/<config>/` for comparison with `expected/<config>/`. Use Beyond Compare or similar diff tool to analyze differences.

### Special Cases Test Structure

For testing formatter settings in isolation (without affecting all standard configurations), use the SpecialCases structure in `UnitTestsFormatter/Testcases/SpecialCases/`:

```
SpecialCases/
  <setting-name>/
    input/          # Input Pascal files
    expected/       # Expected output for this specific configuration
    output/         # Actual output (only written when tests fail)
```

Each special case has its own settings adjuster function in `DelForExSpecialCasesTests.pas`:

```pascal
procedure AdjustSettings_<SettingName>(var _cfg: TCodeFormatterEngineSettings);
begin
  _cfg.<SettingName> := True;  // or other value
end;
```

To add a new special case:
1. Create the directory structure under `Testcases/SpecialCases/<name>/`
2. Add input test files to `input/`
3. Add expected output files to `expected/`
4. Add an `AdjustSettings_<name>` procedure in `DelForExSpecialCasesTests.pas`
5. Register it in `CreateSpecialCasesSuite` with `TSpecialCaseTestSuite.Create('<name>', AdjustSettings_<name>)`

### Formatter Engine

The core formatting engine is in `Source/Formatter/Engine/GX_CodeFormatterFormatter.pas`. The main class `TCodeFormatterFormatter` generates formatted Pascal code from a token collection.

### Verifying Formatter Changes

After modifying the formatter source code:

1. **Rebuild the test runner:**
```bash
cd UnitTestsFormatter
bash _buildproject.sh
```

2. **Run the tests:**
```bash
cd UnitTestsFormatter
bash _runformatterunittests.sh
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

**CRITICAL:** NEVER modify or revert expected test files without explicit user approval. ALWAYS ask first and show the differences. The user needs to verify that the new formatter behavior is correct before test expectations are changed. This applies to both updating expected files AND reverting changes to them.

**Tool usage:** Use the Edit tool for file modifications to preserve encoding. Tools like `sed`, `grep`, `cat`, etc. can be used for reading files and filtering output, but not for modifying files in place.

### Workflow for Adding New Formatter Options

When adding a new formatter option (e.g., a new boolean setting):

**CRITICAL: Create test cases FIRST, before changing any code!**

1. **Create test input file** in `UnitTestsFormatter/Testcases/input/` (e.g., `testfile_MyNewFeature.pas`)
2. **Run tests with the ORIGINAL formatter** to generate baseline expected files:
   ```bash
   cd UnitTestsFormatter
   bash _runformatterunittests.sh
   ```
3. **Copy the output files to expected directories** for all configurations where the new feature will be disabled by default:
   ```bash
   for config in borland default delforex headwork twm; do
     cp Testcases/output/$config/testfile_MyNewFeature.pas Testcases/expected/$config/
   done
   ```
4. **Now implement the formatter changes** (settings, UI, formatter logic)
5. **Enable the new setting** in `AdjustSettings_Special` in `UnitTestsFormatter/source/DelForExTestFiles.pas`
6. **Rebuild and run tests** - the new test will fail for the `special` config (expected)
7. **Copy the special config output** to expected:
   ```bash
   cp Testcases/output/special/testfile_MyNewFeature.pas Testcases/expected/special/
   ```
8. **Update any existing tests affected** by the new feature (check `output/special/` for other files)
9. **Run final test verification** to ensure all ~1380 tests pass (excluding expected `failure_*` test failures)

**Why create tests first?** This workflow ensures you capture the correct baseline behavior using the unmodified formatter. If you change the code first, you'll need to manually recreate what the original output should have been, which is error-prone.

**Alternative: Use SpecialCases instead of modifying all configurations.** If the new option should be isolated from the standard test configurations, use the `SpecialCases` structure (see above) instead of enabling it in `AdjustSettings_Special`. This avoids updating expected output for all standard configurations.

### Smoke-Testing New Formatter Options

After implementing a new formatter option, temporarily enable it in the `special` configuration to see how it affects all existing test files:

1. **Temporarily add the setting** to `AdjustSettings_Special` in `UnitTestsFormatter/source/DelForExTestFiles.pas`
2. **Rebuild and run tests** — many tests will fail (expected)
3. **Examine the output files** in `Testcases/output/special/` to verify the formatting is correct across diverse real-world code patterns
4. **Revert the temporary change** and clean up `Testcases/output/special/`
5. **Rebuild and run tests** to confirm all tests pass again

This technique is valuable for catching edge cases that targeted test files may miss. For example, when implementing assignment alignment, this caught a bug where `for var I: Integer := 0` (typed inline var in for-loop) was incorrectly aligned because the backward scan for the `for` keyword didn't skip past the `:` token between the variable name and type.

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
- **DDetours** - Function hooking library (supports Win32 and Win64)
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

GExperts registers with the Delphi IDE via registry:

**32-bit IDE:**
```
HKEY_CURRENT_USER\Software\Embarcadero\BDS\<version>\Experts
GExperts = <path to DLL>
```

**64-bit IDE:**
```
HKEY_CURRENT_USER\Software\Embarcadero\BDS\<version>\Experts X64
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
- `BlankBeforeUses` - Whether to add a blank line before `uses` clauses
- `IndentCompDirectives` - How compiler directives are indented (None/FollowCode/Independent)
- `DoubleIndentIfCondition` - Double indent for multi-line if conditions

For detailed documentation of formatter features, fixes, and implementation notes, see [FORMATTER_CHANGES.md](FORMATTER_CHANGES.md).

## Win64 Limitations

Some experts are not yet available in the 64-bit build:
- **Filter Exceptions** - Requires different debugger BPL names (`Win64DebugIde*.bpl` vs `Win32DebugIde*.bpl`); the exported function names need to be verified
