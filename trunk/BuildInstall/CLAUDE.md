# BuildInstall Directory

This directory contains the installer creation infrastructure for GExperts.

## Structure

| File/Directory | Purpose |
|----------------|---------|
| `GExperts.iss` | Main Inno Setup script (32-bit) - supports all Delphi versions from D6 to RS13 |
| `GExperts64.iss` | Simplified Inno Setup script (64-bit) - only supports RS13 |
| `MakeInstallers.cmd` | Builds installers for ALL Delphi versions |
| `MakeD12Installer.cmd` | Builds only RS12 installer (convenience script) |
| `MakeD13Installer.cmd` | Builds only RS13 32-bit installer (convenience script) |
| `MakeD13_64Installer.cmd` | Builds the RS13 64-bit installer |
| `_makeinstallers.sh` | WSL script to build all 32-bit installers |
| `_make64installer.sh` | WSL script to build the 64-bit installer |
| `GExperts.fbp6/.fbp7` | FinalBuilder project files (legacy build automation) |
| `InnoSetup_5.6.1/` | Bundled Inno Setup installer for reference |
| `Output/` | Generated installers and release archives |

## Inno Setup Scripts

### GExperts.iss (32-bit)

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

### GExperts64.iss (64-bit)

Simplified script only supporting RS130 (RAD Studio 13). Currently not integrated into the automated build process.

## Build Commands

**Build all 32-bit installers:**
```cmd
MakeInstallers.cmd
```

**Build single 32-bit version:**
```cmd
MakeD13Installer.cmd
```

**Build 64-bit installer:**
```cmd
MakeD13_64Installer.cmd
```

**Build from WSL:**
```bash
bash _makeinstallers.sh    # All 32-bit installers
bash _make64installer.sh   # 64-bit installer for RS13
```

The scripts:
1. Read version from `Projects/GExperts_version.ini`
2. Get current date via WMIC
3. Call Inno Setup compiler with appropriate defines
4. Output to `Output/` directory

## Files Included in Installers

- Main DLL (`GExperts{DLLSuffix}.dll`)
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
- `HKCU\Software\{Company}\{IDE}\{Version}.0\Experts\GExperts` - DLL path (if IDE was run)
- `HKLM\Software\{Company}\{IDE}\{Version}.0\Experts\GExperts` - DLL path (always)
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

- **64-bit built separately**: The 64-bit installer is built with `MakeD13_64Installer.cmd`, not included in `MakeInstallers.cmd`
- **Requires Inno Setup 5**: Scripts expect `C:\Program Files (x86)\Inno Setup 5\ISCC.exe`
