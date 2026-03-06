// the actual code formatter expert, called either from GX_Formatter or GX_eCodeFormatter
// Original Author:     Thomas Mueller (http://blog.dummzeuch.de)
// Contributors:
// * Ulrich Gerhardt - 2007-08-11: added determining the settings via an external .ini file
// * Ulrich Gerhardt - 2009-09-06: support for stand alone formatter
unit GX_CodeFormatterExpert;

{$I GX_CondDefine.inc}

interface

uses
  Classes,
  SysUtils,
  Controls,
  GX_StringList,
  GX_ConfigurationInfo,
  GX_CodeFormatterEngine;

type
  TCodeFormatterExpert = class
  private
    FEngine: TCodeFormatterEngine;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure Configure(_Owner: TWinControl);
    ///<summary>
    /// @returns True if formatting was actually done </summary>
    function Execute: Boolean;
    procedure InternalLoadSettings(_Settings: IExpertSettings);
    procedure InternalSaveSettings(_Settings: IExpertSettings);
    ///<summary>
    /// @returns True, if the captialization list was changed </summary>
    function AddToCapitalization(const _Identifier: TGXUnicodeString): Boolean;
    procedure SaveCapitalization;
    property Engine: TCodeFormatterEngine read FEngine;
  end;

implementation

uses
  ToolsAPI,
  u_dzStringUtils,
  u_dzMiscUtils,
  GX_OtaUtils,
  GX_Logging,
  GX_CodeFormatterGXConfigWrapper,
  GX_CodeFormatterTypes,
  GX_CodeFormatterConfig,
  GX_CodeFormatterSourceLineList,
  GX_CodeFormatterBookmarks,
  GX_CodeFormatterBreakpoints,
  GX_CodeFormatterDefaultSettings,
  GX_CodeFormatterConfigHandler,
  GX_CodeFormatterSettings,
  GX_MessageBox,
  GX_GenericUtils;

var
  TheModuleLogger: IgxLogger = nil;

function Logger: IgxLogger;
begin
  if not Assigned(TheModuleLogger) then
    TheModuleLogger := CreateModuleLogger('Formatter');
  Result := TheModuleLogger;
end;

{ TCodeFormatterExpert }

function TCodeFormatterExpert.AddToCapitalization(const _Identifier: TGXUnicodeString): Boolean;
var
  Idx: Integer;
  sl: TGXUnicodeStringList;
  old: TGXUnicodeString;
begin
  sl := FEngine.Settings.CapNames;
  if sl.Find(_Identifier, Idx) then begin
    old := sl[Idx];
    Result := (old <> _Identifier);
    if Result then begin
      sl.Delete(Idx);
    end;
  end else
    Result := True;
  if Result then
    sl.Add(_Identifier);
end;

procedure TCodeFormatterExpert.Configure(_Owner: TWinControl);
begin
  TfmCodeFormatterConfig.Execute(_Owner, FEngine.Settings);
end;

constructor TCodeFormatterExpert.Create;
var
  Settings: TCodeFormatterEngineSettings;
begin
  inherited Create;

  Settings := BorlandDefaults;
  FEngine := TCodeFormatterEngine.Create;
  FEngine.Settings.Settings := Settings;
end;

destructor TCodeFormatterExpert.Destroy;
begin
  try
    SaveCapitalization;
  except
    // ignore any errors in the destructor
  end;

  FreeAndNil(FEngine);

  inherited;
end;

procedure TCodeFormatterExpert.SaveCapitalization;
var
  Settings: TCodeFormatterSettings;
  Timestamp: TDateTime;
begin
  Settings := FEngine.Settings;
  Timestamp := Settings.CapFileTimestamp;
  TCodeFormatterConfigHandler.WriteCaptialization(Settings.CapitalizationFile, Settings.CapNames, Timestamp);
  Settings.CapFileTimestamp := Timestamp;
end;

type
  TCodeFormatterDone = class(TGxMsgBoxAdaptor)
  protected
    function GetMessage: string; override;
  end;

{ TCodeFormatterDone }

function TCodeFormatterDone.GetMessage: string;
resourcestring
  str_FileHasBeenFormatted = 'The file has been reformatted.';
begin
  Result := str_FileHasBeenFormatted;
end;

function TCodeFormatterExpert.Execute: Boolean;
resourcestring
  str_NoEditor = 'No source editor';
  str_UnsupportedFileTypeS = 'Unsupported file type: %s';
  str_UnableToGetContentsS = 'Unable to get contents of %s';
var
  SourceEditor: IOTASourceEditor;
  FileName: string;
  FullTextStr: TGXUnicodeString;
  FullText: TSourceLineList;
  Bookmarks: TBookmarkHandler;
  Breakpoints: TBreakpointHandler;
  i: Integer;
  TempSettings: TCodeFormatterSettings;
  OrigSettings: TCodeFormatterEngineSettings;
  SettingsName: string;
  BlockStart: TOTAEditPos;
  BlockEnd: TOTAEditPos;
  SelStart: Integer;
  SelLength: Integer;
  FormatSelection: Boolean;
  Logger: IgxLogger;
begin
  Logger := CreateModuleLogger('CodeFormatter');
  Logger.MethodEnter('TCodeFormatterExpert.Execute');

  Result := False;

  if not Ota.TryGetCurrentSourceEditor(SourceEditor) then
    raise ECodeFormatter.Create(str_NoEditor);

  FileName := SourceEditor.FileName;
  if not (IsPascalSourceFile(FileName) or IsDelphiPackage(FileName) or FileMatchesExtension(FileName, '.tpl')) then
    raise ECodeFormatter.CreateFmt(str_UnsupportedFileTypeS, [ExtractFileName(FileName)]);

  Logger.InfoFmt('Formatting requested for "%s"', [FileName]);

  if not Ota.GetActiveEditorTextAsUnicodeString(FullTextStr, False) then
    raise ECodeFormatter.CreateFmt(str_UnableToGetContentsS, [FileName]);

  if FullTextStr = '' then
    Exit; //==>

  InitializeNil(TempSettings, FullText, Breakpoints, Bookmarks);
  try
    Logger.Info('TCodeFormatterExpert.Execute:Save Breakpoints and bookmarks');
    Bookmarks := TBookmarkHandler.Create;
    Bookmarks.SaveItems;
    Breakpoints := TBreakpointHandler.Create;
    Breakpoints.SaveItems;

    Logger.Info('TCodeFormatterExpert.Execute:Determine settings');
    SettingsName := Trim(FEngine.GetSettingsName(FileName, FullTextStr));
    if SettingsName = '-' then begin
      Logger.Info('Ignoring request, no settings name available');
    end else if SettingsName = '' then begin
      Logger.Info('Use default settings');
    end else begin
      Logger.InfoFmt('Use settings "%s"', [SettingsName]);
      TempSettings := TCodeFormatterSettings.Create;
      if TCodeFormatterConfigHandler.GetDefaultConfig(SettingsName, TempSettings,
        GetDefaultCapitalizationFilename) then begin
        OrigSettings := FEngine.Settings.Settings;
        FEngine.Settings.Settings := TempSettings.Settings;
      end else
        FreeAndNil(TempSettings);
    end;

    Logger.Info('TCodeFormatterExpert.Execute:Format selection ?');
    FormatSelection := GxOtaGetSelection(
      Ota.GetTopMostEditView(SourceEditor), BlockStart, BlockEnd, SelStart, SelLength);
    if FormatSelection then
      FormatSelection := (SelLength > 0) and (BlockStart.Line < BlockEnd.Line);

    if not FormatSelection then begin
      Logger.Info('TCodeFormatterExpert.Execute:Formatting whole file');
      FullText := TSourceLineList.Create(FullTextStr, 0, -1);
    end else begin
      Logger.Info('TCodeFormatterExpert.Execute:Prepare to format selection');
      BlockStart.Col := 1;
      if BlockEnd.Col > 1 then begin
        BlockEnd.Col := 1;
        BlockEnd.Line := BlockEnd.Line + 1;
      end;
      Assert(BlockEnd.Line > 0, 'Blockend.Line <= 0');
      Assert(BlockStart.Line > 0, 'BlockStart.Line <= 0');
      FullText := TSourceLineList.Create(FullTextStr, BlockStart.Line, BlockEnd.Line);
    end;
    FullTextStr := ''; // might save some memory
    Logger.Info('TCodeFormatterExpert.Execute:Executing formatter');
    if FEngine.Execute(FullText) then begin
      if FormatSelection then begin
        Logger.Info('TCodeFormatterExpert.Execute:Replace selection only');
        FullTextStr := FullText.GetFormattedBlock;
        GxOtaSelectBlock(SourceEditor, BlockStart, BlockEnd);
        GxOtaReplaceSelection(SourceEditor, 0, FullTextStr);
      end else begin
        Logger.Info('TCodeFormatterExpert.Execute:Replace full text');
        Ota.ReplaceEditorTextWithUnicodeString(SourceEditor, FullText.Text);
      end;
      Logger.Info('TCodeFormatterExpert.Execute:Restore breakpoints and bookmarks');
      Breakpoints.RestoreItems(FullText);
      Bookmarks.RestoreItems(FullText);
      Logger.Info('TCodeFormatterExpert.Execute:Force repaints');
      for i := 0 to SourceEditor.EditViewCount - 1 do
        SourceEditor.EditViews[i].Paint;
      ShowGxMessageBox(TCodeFormatterDone);
      Result := True;
    end else begin
      Logger.Info('FEngine.Exeucte returned false, not changing anything');
    end;
    Logger.Info('TCodeFormatterExpert.Execute:Cleanup');
  finally
    FreeAndNil(Breakpoints, Bookmarks, FullText);
    if Assigned(TempSettings) then begin
      FreeAndNil(TempSettings);
      FEngine.Settings.Settings := OrigSettings;
    end;
  end;
end;

procedure TCodeFormatterExpert.InternalLoadSettings(_Settings: IExpertSettings);
var
  Reader: IConfigReader;
begin
  Reader := TGxConfigWrapper.Create(_Settings);
  TCodeFormatterConfigHandler.ReadSettings(Reader, FEngine.Settings,
    GetDefaultCapitalizationFilename);
end;

procedure TCodeFormatterExpert.InternalSaveSettings(_Settings: IExpertSettings);
var
  Writer: IConfigWriter;
begin
  Writer := TGxConfigWrapper.Create(_Settings);
  TCodeFormatterConfigHandler.WriteSettings(Writer, FEngine.Settings);
end;

end.

