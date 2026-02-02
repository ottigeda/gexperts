# BuildInstall Directory

This directory contains the installer creation infrastructure for GExperts.

## Structure

| File/Directory | Purpose |
|----------------|---------|
| `GExperts.iss` | Main Inno Setup script - supports all Delphi versions from D6 to RS13 |
| `MakeInstallers.cmd` | Builds installers for ALL Delphi versions |
| `MakeD12Installer.cmd` | Builds only RS12 installer (convenience script) |
| `MakeD13Installer.cmd` | Builds only RS13 installer (convenience script) |
| `_makeinstallers.sh` | WSL script to build all installers |
| `GExperts.fbp6/.fbp7` | FinalBuilder project files (legacy build automation) |
| `InnoSetup_5.6.1/` | Bundled Inno Setup installer for reference |
| `Output/` | Generated installers and release archives |

## Inno Setup Scripts

### GExperts.iss

The main installer script supports all Delphi versions via preprocessor defines:

- **Delphi 6-7**: `Delphi6`, `Delphi7` (Borland registry)
- **Delphi 2005-2007**: `Delphi2005`, `BDS2006`, `Delphi2007` (Borland registry)
- **RAD Studio 2009-2010**: `RS2009`, `RS2010` (CodeGear registry)
- **RAD Studio XE1-XE8**: `RSXE1` through `RSXE8` (Embarcadero registry)
- **RAD Studio 10.x**: `RS100`, `RS101`, `RS102`, `RS103`, `RS104`
- **RAD Studio 11-13**: `RS110`, `RS120`, `RS130`

Usage:
```cmd
iscc.exe GExperts.iss /dRS130 /dVersion=1.3.26 /fGExperts_RS13_1.3.26_2025-01-31
```

### RS13 Combined 32-bit/64-bit Installer

Starting with RAD Studio 13, the installer includes both 32-bit and 64-bit DLLs:
- `GExpertsRS130.dll` (32-bit) - always installed and registered
- `GExperts64RS130.dll` (64-bit) - always installed, registration optional

The installer shows a checkbox "Also register for 64-bit IDE" (checked by default) that controls whether the 64-bit DLL is registered in `Experts X64`.

## Build Commands

**Build all installers:**
```cmd
MakeInstallers.cmd
```

**Build single version:**
```cmd
MakeD13Installer.cmd       # RS13 (includes both 32-bit and 64-bit)
```

**Build from WSL:**
```bash
bash _makeinstallers.sh    # All installers
```

The scripts:
1. Read version from `Projects/GExperts_version.ini`
2. Get current date via WMIC
3. Call Inno Setup compiler with appropriate defines
4. Output to `Output/` directory

## Files Included in Installers

- Main DLL (`GExperts{DLLSuffix}.dll`)
- For RS13: Also includes 64-bit DLL (`GExperts64RS130.dll`)
- Standalone utilities:
  - `GExpertsPeInformation.exe` - PE file analyzer
  - `ExpertManager.exe` - Expert registration tool
  - `GExpertsDebugWindow.exe` - Debug message viewer
  - `GExpertsGrep.exe` - Search tool
  - `GExpertsFormatter.exe` - Code formatter
- `GExperts.chm` - Help file
- `DbugIntf.pas` - Debug tracing unit for integration
- Formatter settings INI files
- Component renaming INI files

## Registry Entries

The installer creates:

**32-bit IDE (all versions):**
- `HKCU\Software\{Company}\{IDE}\{Version}.0\Experts\GExperts` - DLL path (if IDE was run)
- `HKLM\Software\{Company}\{IDE}\{Version}.0\Experts\GExperts` - DLL path (always)

**64-bit IDE (RS13 only, optional):**
- `HKCU\Software\{Company}\{IDE}\{Version}.0\Experts X64\GExperts` - 64-bit DLL path
- `HKLM\Software\{Company}\{IDE}\{Version}.0\Experts X64\GExperts` - 64-bit DLL path

- `HKCU\Software\GExperts\Debug\FilePath` - Debug Window path

Company varies by IDE version: Borland (D6-2007), CodeGear (2009-2010), Embarcadero (XE1+)

## Output Naming Convention

Generated installers follow this pattern:
```
GExperts_{IDEPrefix}_{Version}_{Date}.exe
```

Examples:
- `GExperts_D7_1.3.25_2025-05-18.exe`
- `GExperts_RS13_1.3.26_2025-09-27.exe`

## Known Limitations

- **Requires Inno Setup 5**: Scripts expect `C:\Program Files (x86)\Inno Setup 5\ISCC.exe`
