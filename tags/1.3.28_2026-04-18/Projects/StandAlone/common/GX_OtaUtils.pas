///<summary>
/// Declares dummies for functions from GX_OtaUtils that return values appropriate for
/// failure (e.g. empty strings, Nil, False) so there are no compile errors for
/// stand alone tools that do not have access to the OTAPI </summary>
unit GX_OtaUtils;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils,
  Classes,
  Graphics,
  Forms,
  ToolsAPI,
  GX_GenericUtils,
  GX_StringList;

type
  Ota = class
  public
    class function InitTheming(AForm: TCustomForm): Boolean;
    class function GetEditWriterForSourceEditor(SourceEditor: IOTASourceEditor = nil): IOTAEditWriter;
    class function GetActiveEditorText(Lines: TGXUnicodeStringList; UseSelection: Boolean = True): Boolean;
    // Returns an empty string
    class function GetCurrentIdent: string;
    // always returns False
    class function IdeStyleIsDark: Boolean;
    // always returns nil
    class function GetFormEditorFromModule(const Module: IOTAModule): IOTAFormEditor;
    // always returns nil
    class function GetSourceEditorFromModule(Module: IOTAModule; const FileName: string = ''): IOTASourceEditor;
    // always returns an empty string
    class function SelectedComponentName: string;
    class procedure LoadFileToUnicodeStrings(const FileName: string; Data: TGXUnicodeStringList); overload;
    class procedure LoadFileToUnicodeStrings(const FileName: string; Data: TGXUnicodeStringList; var WasBinary: Boolean); overload;
    class procedure GetEditorFont(const AFont: TFont; ASizeAdjust: Integer = 0);
    class function GetIdeBaseRegistryKey: string;
    class function GetModule(const _Filename: string): IOTAModule;
    class function GetBaseModuleFileName(const FileName: string): string;
    class function IsFileOpen(const AFileName: string; UseBase: Boolean = False): Boolean;
    class function FileOrModuleExists(const AFileName: string; UseBase: Boolean = False): Boolean;
    class function GetProjectGroup: IOTAProjectGroup;
    class function GetProjectFileName(Project: IOTAProject; NormalizeBdsProj: Boolean = False): string;
    class function GetCurrentMapFileName(out MapFile: string): Boolean;
    class procedure GetAllPossiblePaths(Paths: TStrings; Project: IOTAProject = nil; AdditionalPaths: TStrings = nil);
    class function GetCurrentSourceFile: string;
    class function GetCurrentProject: IOTAProject;
    class function CurrentProjectIsNativeCpp: Boolean;
    class function CurrentProjectIsCSharp: Boolean;
    class procedure GoToFileLineColumn(const FileName: string; Line: Integer;
      StartColumn: Integer = 0; StopColumn: Integer = 0; ShowInMiddle: Boolean = True);
    class function GetCurrentSelection(IncludeTrailingCRLF: Boolean = True): string;
    class function GetCurrentEditorAsSourceEditor: IOTASourceEditor;
    class function GetCurrentProjectName: string;
    class function GetProjectGroupFileName: string;
    class function GetFileNameOfCurrentModule: string;
    class function HaveCPPSupport: Boolean;
    class function HaveCSharpSupport: Boolean;
    class function GetIDEProductIdentifier: string;
    // always returns False
    class function ThemingEnabled: Boolean;
    // always returns False
    class function TryGetCurrentSourceEditor(out SourceEditor: IOTASourceEditor): Boolean;
    // always returns gxpNone
    class function GetCurrentSyntaxHighlighter(SourceEditor: IOTASourceEditor = nil): TGXSyntaxHighlighter;
    // always returns an empty string
    class function GetCurrentProjectFileName(NormalizeBdsProj: Boolean = False): string;
  end;

///<summary>
/// @returns True </summary>
function IsStandAlone: Boolean;

///<summary>
/// Fake function, always returs nil </summary>
function GxOtaGetModule(const _Filename: string): IOTAModule;

// Transform dfm, hpp, etc. references to the base .pas/cpp file name
function GxOtaGetBaseModuleFileName(const FileName: string): string;

// Get the configured editor font.
procedure GxOtaGetEditorFont(const AFont: TFont; ASizeAdjust: Integer = 0);

// Returns the IOTASourceEditor interface for a module
// if there is a file that supports one; returns nil
// if there is no IOTASourceEditor
function GxOtaGetSourceEditorFromModule(Module: IOTAModule; const FileName: string = ''): IOTASourceEditor;

procedure GXOtaLoadFileToUnicodeStrings(const FileName: string; Data: TGXUnicodeStringList;
  var WasBinary: Boolean); overload;
procedure GXOtaLoadFileToUnicodeStrings(const FileName: string; Data: TGXUnicodeStringList); overload;

// raises an exception
function GxOtaGetEditWriterForSourceEditor(SourceEditor: IOTASourceEditor = nil): IOTAEditWriter;

// always returns False
function GxOtaIsFileOpen(const AFileName: string; UseBase: Boolean = False): Boolean;

function GXOtaGetIdeBaseRegistryKey: string;

// Determine is a file exists or the file's module is currently loaded in the IDE
function GxOtaFileOrModuleExists(const AFileName: string; UseBase: Boolean = False): Boolean;

// returns Nil
function GxOtaGetProjectGroup: IOTAProjectGroup;

///<summary>
/// @returns an empty string </summary>
function GxOtaGetProjectFileName(Project: IOTAProject; NormalizeBdsProj: Boolean = False): string;

///<summary>
/// @returns False </summary>
function GxOtaGetCurrentMapFileName(out MapFile: string): Boolean;

///<summary>
/// does nothing </summary>
procedure GxOtaGetAllPossiblePaths(Paths: TStrings; Project: IOTAProject = nil; AdditionalPaths: TStrings = nil);

// Returns an empty string
function GxOtaGetCurrentSourceFile: string;

// returns Nil
function GxOtaGetCurrentProject: IOTAProject;

function GxOtaCurrentProjectIsNativeCpp: Boolean;

function GxOtaCurrentProjectIsCSharp: Boolean;

// returns Nil
function GxOtaGetFormEditorFromModule(const Module: IOTAModule): IOTAFormEditor;

procedure GxOtaGoToFileLineColumn(const FileName: string; Line: Integer;
  StartColumn: Integer = 0; StopColumn: Integer = 0; ShowInMiddle: Boolean = True);

// returns False
function GxOtaGetActiveEditorText(Lines: TGXUnicodeStringList; UseSelection: Boolean = True): Boolean;

// Returns an empty string
function GxOtaGetCurrentSelection(IncludeTrailingCRLF: Boolean = True): string;

// returns Nil
function GxOtaGetCurrentEditorAsSourceEditor: IOTASourceEditor;

///<sumamry>
/// @returns an emptry string if no or more than 1 component is selected. </summary>
function GxOtaSelectedComponentName: string;

// Returns an empty string
function GxOtaGetCurrentIdent: string;

// Returns an empty string
function GxOtaGetCurrentProjectName: string;

// Returns an empty string
function GxOtaGetProjectGroupFileName: string;

// Returns an empty string
function GxOtaGetFileNameOfCurrentModule: string;

// returns False
function GxOtaHaveCPPSupport: Boolean;

// returns False
function GxOtaHaveCSharpSupport: Boolean;

function GxOtaGetIDEProductIdentifier: string;

type
  TThemeChangedNotifier = class
  public
    constructor Create(_OnThemeChanged: TNotifyEvent);
  public
    // always returns True
    function AddNotifierToIDE: Boolean;
    // always returns True
    function RemoveNotifierFromIDE: Boolean;
  end;

implementation

uses
  GX_VerDepConst;

function IsStandAlone: Boolean;
begin
  Result := True;
end;

function GxOtaGetModule(const _Filename: string): IOTAModule;
begin
  Result := nil;
end;

function GxOtaGetBaseModuleFileName(const FileName: string): string;
var
  AltName: string;
begin
  Result := FileName;
  if IsForm(FileName) then begin
    AltName := ChangeFileExt(FileName, '.cpp');
    if FileExists(AltName) then
      Result := AltName;
    AltName := ChangeFileExt(FileName, '.pas');
    if FileExists(AltName) then
      Result := AltName;
  end;
end;

function GxOtaGetSourceEditorFromModule(Module: IOTAModule; const FileName: string = ''): IOTASourceEditor;
begin
  Result := nil;
end;

class procedure Ota.LoadFileToUnicodeStrings(const FileName: string; Data: TGXUnicodeStringList;
  var WasBinary: Boolean);
begin
  Assert(Assigned(Data));
  Data.Clear;
  if IsEmpty(FileName) then
    raise Exception.Create('Blank filenames are not allowed');

  LoadDiskFileToUnicodeStrings(FileName, Data, WasBinary)
end;

class procedure Ota.LoadFileToUnicodeStrings(const FileName: string; Data: TGXUnicodeStringList);
var
  WasBinary: Boolean;
begin
  Ota.LoadFileToUnicodeStrings(FileName, Data, WasBinary);
end;

procedure GXOtaLoadFileToUnicodeStrings(const FileName: string; Data: TGXUnicodeStringList;
  var WasBinary: Boolean);
begin
  Ota.LoadFileToUnicodeStrings(FileName, Data, WasBinary);
end;

procedure GXOtaLoadFileToUnicodeStrings(const FileName: string; Data: TGXUnicodeStringList);
begin
  Ota.LoadFileToUnicodeStrings(FileName, Data);
end;

function GxOtaGetEditWriterForSourceEditor(SourceEditor: IOTASourceEditor = nil): IOTAEditWriter;
begin
  raise Exception.Create('Stand alone version does not support ToolsAPI calls (GxOtaGetEditWriterForSourceEditor)');
end;

function GxOtaIsFileOpen(const AFileName: string; UseBase: Boolean = False): Boolean;
begin
  Result := False;
end;

function GXOtaGetIdeBaseRegistryKey: string;
begin
  Result := 'Software\' + CompilerDefinedProductRegistryKey;

  if Length(Result) > 0 then begin
    // Windows 2000 is allergic to a leading backslash
    // in the registry key - NT4, for instance, is not.
    if Result[1] = '\' then
      Delete(Result, 1, 1);

    Assert(Result[Length(Result)] <> '\');
  end;
end;

function GxOtaFileOrModuleExists(const AFileName: string; UseBase: Boolean): Boolean;
begin
  Result := FileExists(AFileName);
end;

function GxOtaGetProjectGroup: IOTAProjectGroup;
begin
  Result := nil;
end;

function GxOtaGetProjectFileName(Project: IOTAProject; NormalizeBdsProj: Boolean = False): string;
begin
  Result := '';
end;

function GxOtaGetCurrentMapFileName(out MapFile: string): Boolean;
begin
  Result := False;
end;

procedure GxOtaGetAllPossiblePaths(Paths: TStrings; Project: IOTAProject = nil; AdditionalPaths: TStrings = nil);
begin
  // do nothing
end;

function GxOtaGetCurrentSourceFile: string;
begin
  Result := '';
end;

function GxOtaGetCurrentProject: IOTAProject;
begin
  Result := nil;
end;

function GxOtaCurrentProjectIsNativeCpp: Boolean;
begin
  Result := False;
end;

function GxOtaCurrentProjectIsCSharp: Boolean;
begin
  Result := False;
end;

function GxOtaGetFormEditorFromModule(const Module: IOTAModule): IOTAFormEditor;
begin
  Result := nil;
end;

procedure GxOtaGoToFileLineColumn(const FileName: string; Line: Integer; StartColumn: Integer = 0;
  StopColumn: Integer = 0; ShowInMiddle: Boolean = True);
begin
  // do nothing
end;

function GxOtaGetActiveEditorText(Lines: TGXUnicodeStringList; UseSelection: Boolean = True): Boolean;
begin
  Result := False;
end;

function GxOtaGetCurrentSelection(IncludeTrailingCRLF: Boolean = True): string;
begin
  Result := '';
end;

function GxOtaGetCurrentEditorAsSourceEditor: IOTASourceEditor;
begin
  Result := nil;
end;

function GxOtaSelectedComponentName: string;
begin
  Result := '';
end;

function GxOtaGetCurrentIdent: string;
begin
  Result := '';
end;

function GxOtaGetCurrentProjectName: string;
begin
  Result := '';
end;

procedure GxOtaGetEditorFont(const AFont: TFont; ASizeAdjust: Integer = 0);
begin
  // do nothing for now
end;

function GxOtaGetProjectGroupFileName: string;
begin
  Result := '';
end;

function GxOtaGetFileNameOfCurrentModule: string;
begin
  Result := '';
end;

function GxOtaHaveCPPSupport: Boolean;
begin
  Result := False;
end;

function GxOtaHaveCSharpSupport: Boolean;
begin
  Result := False;
end;

function GxOtaGetIDEProductIdentifier: string;
begin
  Result := '';
end;

{ Ota }

class function Ota.GetActiveEditorText(Lines: TGXUnicodeStringList; UseSelection: Boolean): Boolean;
begin
  Result := False;
end;

class function Ota.GetCurrentIdent: string;
begin
  Result := '';
end;

class function Ota.GetEditWriterForSourceEditor(SourceEditor: IOTASourceEditor): IOTAEditWriter;
begin
  raise Exception.Create('Stand alone version does not support ToolsAPI calls (Ota.GetEditWriterForSourceEditor)');
end;

class function Ota.IdeStyleIsDark: Boolean;
begin
  Result := False;
end;

class function Ota.InitTheming(AForm: TCustomForm): Boolean;
begin
  Result := False;
end;

class function Ota.GetFormEditorFromModule(const Module: IOTAModule): IOTAFormEditor;
begin
  Result := nil;
end;

class function Ota.GetSourceEditorFromModule(Module: IOTAModule; const FileName: string): IOTASourceEditor;
begin
  Result := nil;
end;

class function Ota.SelectedComponentName: string;
begin
  Result := '';
end;

class procedure Ota.GetEditorFont(const AFont: TFont; ASizeAdjust: Integer);
begin
  // do nothing in standalone
end;

class function Ota.GetIdeBaseRegistryKey: string;
begin
  Result := GXOtaGetIdeBaseRegistryKey;
end;

class function Ota.GetModule(const _Filename: string): IOTAModule;
begin
  Result := GxOtaGetModule(_Filename);
end;

class function Ota.GetBaseModuleFileName(const FileName: string): string;
begin
  Result := GxOtaGetBaseModuleFileName(FileName);
end;

class function Ota.IsFileOpen(const AFileName: string; UseBase: Boolean): Boolean;
begin
  Result := GxOtaIsFileOpen(AFileName, UseBase);
end;

class function Ota.FileOrModuleExists(const AFileName: string; UseBase: Boolean): Boolean;
begin
  Result := GxOtaFileOrModuleExists(AFileName, UseBase);
end;

class function Ota.GetProjectGroup: IOTAProjectGroup;
begin
  Result := GxOtaGetProjectGroup;
end;

class function Ota.GetProjectFileName(Project: IOTAProject; NormalizeBdsProj: Boolean): string;
begin
  Result := GxOtaGetProjectFileName(Project, NormalizeBdsProj);
end;

class function Ota.GetCurrentMapFileName(out MapFile: string): Boolean;
begin
  Result := GxOtaGetCurrentMapFileName(MapFile);
end;

class procedure Ota.GetAllPossiblePaths(Paths: TStrings; Project: IOTAProject; AdditionalPaths: TStrings);
begin
  GxOtaGetAllPossiblePaths(Paths, Project, AdditionalPaths);
end;

class function Ota.GetCurrentSourceFile: string;
begin
  Result := GxOtaGetCurrentSourceFile;
end;

class function Ota.GetCurrentProject: IOTAProject;
begin
  Result := GxOtaGetCurrentProject;
end;

class function Ota.CurrentProjectIsNativeCpp: Boolean;
begin
  Result := GxOtaCurrentProjectIsNativeCpp;
end;

class function Ota.CurrentProjectIsCSharp: Boolean;
begin
  Result := GxOtaCurrentProjectIsCSharp;
end;

class procedure Ota.GoToFileLineColumn(const FileName: string; Line: Integer;
  StartColumn: Integer; StopColumn: Integer; ShowInMiddle: Boolean);
begin
  GxOtaGoToFileLineColumn(FileName, Line, StartColumn, StopColumn, ShowInMiddle);
end;

class function Ota.GetCurrentSelection(IncludeTrailingCRLF: Boolean): string;
begin
  Result := GxOtaGetCurrentSelection(IncludeTrailingCRLF);
end;

class function Ota.GetCurrentEditorAsSourceEditor: IOTASourceEditor;
begin
  Result := GxOtaGetCurrentEditorAsSourceEditor;
end;

class function Ota.GetCurrentProjectName: string;
begin
  Result := GxOtaGetCurrentProjectName;
end;

class function Ota.GetProjectGroupFileName: string;
begin
  Result := GxOtaGetProjectGroupFileName;
end;

class function Ota.GetFileNameOfCurrentModule: string;
begin
  Result := GxOtaGetFileNameOfCurrentModule;
end;

class function Ota.HaveCPPSupport: Boolean;
begin
  Result := GxOtaHaveCPPSupport;
end;

class function Ota.HaveCSharpSupport: Boolean;
begin
  Result := GxOtaHaveCSharpSupport;
end;

class function Ota.GetIDEProductIdentifier: string;
begin
  Result := GxOtaGetIDEProductIdentifier;
end;

{ TThemeChangedNotifier }

constructor TThemeChangedNotifier.Create(_OnThemeChanged: TNotifyEvent);
begin
  inherited Create;
end;

function TThemeChangedNotifier.RemoveNotifierFromIDE: Boolean;
begin
  Result := True;
end;

function TThemeChangedNotifier.AddNotifierToIDE: Boolean;
begin
  Result := True;
end;

class function Ota.ThemingEnabled: Boolean;
begin
  Result := False;
end;

class function Ota.TryGetCurrentSourceEditor(out SourceEditor: IOTASourceEditor): Boolean;
begin
  SourceEditor := nil;
  Result := False;
end;

class function Ota.GetCurrentSyntaxHighlighter(SourceEditor: IOTASourceEditor): TGXSyntaxHighlighter;
begin
  Result := gxpNone;
end;

class function Ota.GetCurrentProjectFileName(NormalizeBdsProj: Boolean): string;
begin
  Result := '';
end;

end.
