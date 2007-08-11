// the actual code formatter expert, called either from GX_Formatter or GX_eCodeFormatter
// Original Author:     Thomas Mueller (http://www.dummzeuch.de)
// Contributors:
// * Ulrich Gerhardt - 2007-08-11: added determining the settings via an external .ini file
unit GX_CodeFormatterExpert;

{$I GX_CondDefine.inc}

interface

uses
  Classes,
  SysUtils,
  GX_ConfigurationInfo,
  GX_CodeFormatterEngine;

type
  TCodeFormatterExpert = class
  private
    FEngine: TCodeFormatterEngine;
    function ShowDoneDialog(ShowDone: Boolean): boolean;
    function GetSettingsName(FileName: string; FullText: TStringList): string;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure Configure;
    procedure Execute;
    procedure InternalLoadSettings(const ConfigurationKey: string; Settings: TGExpertsSettings);
    procedure InternalSaveSettings(const ConfigurationKey: string; Settings: TGExpertsSettings);
  end;

implementation

uses
  IniFiles,
  ToolsApi,
  GX_OtaUtils,
  GX_GenericUtils,
  GX_DbugIntf,
  GX_CodeFormatterGXConfigWrapper,
  GX_CodeFormatterTypes,
  GX_CodeFormatterConfig,
  GX_CodeFormatterBookmarks,
  GX_CodeFormatterBreakpoints,
  GX_CodeFormatterDefaultSettings,
  GX_CodeFormatterConfigHandler,
  GX_CodeFormatterDone,
  GX_CodeFormatterSettings;

procedure XSendDebug(const Msg: string); inline;
begin
  SendDebug('GXFormatter: ' + Msg);
end;

{ TCodeFormatterExpert }

procedure TCodeFormatterExpert.Configure;
begin
  TfmCodeFormatterConfig.Execute(FEngine.Settings);
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
  FreeAndNil(FEngine);
  inherited;
end;

function FileNameMatches(const FileName, Pattern: string): Boolean;
var
  sr: TSearchRec;
begin
  Result := False;
  if FindFirst(Pattern, 0, sr) = 0 then begin
    repeat
      if SameText(FileName, sr.Name) then begin
        Result := True;
        Break;
      end;
    until FindNext(sr) <> 0;
    FindClose(sr);
  end;
end;

function TCodeFormatterExpert.GetSettingsName(FileName: string; FullText: TStringList): string;

  function GetFromConfigFile: string;
  var
    ConfigFileName, ConfiguredFileName, Path: string;
    IniFile: TIniFile;
    ConfiguredFileNames: TStringList;
    i: Integer;
  begin
    Result := '';
    Path := AddSlash(ExtractFilePath(ExpandFileName(FileName)));
    ConfigFileName := Path + 'GXFormatter.ini'; // Do not localize.
    IniFile := TIniFile.Create(ConfigFileName);
    try
      ConfiguredFileNames := TStringList.Create;
      try
        // Read section manually to conserve ordering:
        IniFile.ReadSection('FileSettings', ConfiguredFileNames);
        for i := 0 to Pred(ConfiguredFileNames.Count) do begin
          ConfiguredFileName := ConfiguredFileNames[i];
          if FileNameMatches(ExtractFileName(FileName), Path + ConfiguredFileName) then begin
            Result := IniFile.ReadString('FileSettings', ConfiguredFileName, '');
            {$IFOPT D+}XSendDebug(Format('Settings "%s" from rule %s in %s', [Result, ConfiguredFileName, ConfigFileName])); {$ENDIF}
            Break;
          end;
        end;
      finally
        FreeAndNil(ConfiguredFileNames);
      end;
    finally
      FreeAndNil(IniFile);
    end;
  end;

  function GetFromSource: string;
  const
    cConfigDirective = '{GXFormatter.config='; // Do not localize.
  var
    FirstLine: string;
  begin
    FirstLine := Trim(FullText[0]);
    if SameText(Copy(FirstLine, 1, Length(cConfigDirective)), cConfigDirective) and
      (FirstLine[Length(FirstLine)] = '}') then begin
      Result := Trim(Copy(FirstLine, Length(cConfigDirective) + 1, Length(FirstLine) - Length(cConfigDirective) - 1));
      {$IFOPT D+}XSendDebug(Format('Settings "%s" from in-source directive', [Result])); {$ENDIF}
    end else
      Result := '';
  end;

  function GetSettingNameFor(_Precedence: TConfigPrecedenceEnum; out _Name: string): boolean;
  begin
    case _Precedence of
      cpDirective: begin
        _Name := GetFromSource;
        Result := _Name <> '';
      end;
      cpIniFile: begin
        _Name := GetFromConfigFile;
        Result := _Name <> '';
      end;
      cpMyConfig: begin
        _Name := '';
        Result := true;
      end
    else
      Result := False;
    end;
  end;

var
  i: Integer;
begin
  for i := Low(TOneToThree) to High(TOneToThree) do
    if GetSettingNameFor(FEngine.Settings.ConfigPrecedence[i], Result) then
      exit;
  Result := '';
end;

procedure TCodeFormatterExpert.Execute;
resourcestring
  str_NoEditor = 'No source editor';
  str_UnsupportedFileTypeS = 'Unsupported file type: %s';
  str_UnableToGetContentsS = 'Unable to get contents of %s';
var
  SourceEditor: IOTASourceEditor;
  FileName: string;
  FullText: TStringList;
  Bookmarks: TBookmarkHandler;
  Breakpoints: TBreakpointHandler;
  WasBinary: Boolean;
  i: integer;
  TempSettings: TCodeFormatterSettings;
  OrigSettings: TCodeFormatterEngineSettings;
  SettingsName: string;
begin
  SourceEditor := GxOtaGetCurrentSourceEditor;
  if not Assigned(SourceEditor) then
    raise ECodeFormatter.Create(str_NoEditor);
  FileName := SourceEditor.FileName;
  if not (IsPascalSourceFile(FileName) or IsDelphiPackage(FileName) or FileMatchesExtension(FileName, '.tpl')) then
    raise ECodeFormatter.CreateFmt(str_UnsupportedFileTypeS, [ExtractFileName(FileName)]);

  {$IFOPT D+}XSendDebug(Format('Formatting requested for "%s"', [FileName])); {$ENDIF}
  TempSettings := nil;
  FullText := TStringList.Create;
  try
    if not GxOtaGetFileAsText(Filename, FullText, WasBinary) then
      raise ECodeFormatter.CreateFmt(str_UnableToGetContentsS, [FileName]);
    if FullText.Count = 0 then
      exit;

    Breakpoints := nil;
    Bookmarks := TBookmarkHandler.Create;
    try
      Breakpoints := TBreakpointHandler.Create;
      Breakpoints.SaveItems;
      Bookmarks.SaveItems;
      SettingsName := Trim(GetSettingsName(FileName, FullText));
      if SettingsName <> '-' then begin
        if SettingsName <> '' then begin
          {$IFOPT D+}XSendDebug(Format('Use settings "%s"', [SettingsName])); {$ENDIF}
          TempSettings := TCodeFormatterSettings.Create;
          if TCodeFormatterConfigHandler.GetDefaultConfig(SettingsName, TempSettings) then begin
            OrigSettings := FEngine.Settings.Settings;
            FEngine.Settings.Settings := TempSettings.Settings;
          end else
            FreeAndNil(TempSettings);
        end else
          {$IFOPT D+}XSendDebug('Use default settings'); {$ENDIF}

        if FEngine.Execute(FullText) then begin
          GxOtaReplaceEditorText(SourceEditor, FullText.Text);
          Breakpoints.RestoreItems;
          Bookmarks.RestoreItems;
          for i := 0 to SourceEditor.EditViewCount - 1 do
            SourceEditor.EditViews[i].Paint;
          FEngine.Settings.ShowDoneDialog := ShowDoneDialog(FEngine.Settings.ShowDoneDialog);
        end;
      end else
        {$IFOPT D+}XSendDebug('Ignore request'); {$ENDIF}
    finally
      FreeAndNil(Breakpoints);
      FreeAndNil(Bookmarks);
    end;

  finally
    FreeAndNil(FullText);
    if Assigned(TempSettings) then begin
      FreeAndNil(TempSettings);
      FEngine.Settings.Settings := OrigSettings;
    end;
  end;
end;

procedure TCodeFormatterExpert.InternalLoadSettings(const ConfigurationKey: string; Settings: TGExpertsSettings);
var
  Reader: IConfigReader;
begin
  Reader := TGxConfigWrapper.Create(Settings, ConfigurationKey);
  TCodeFormatterConfigHandler.ReadSettings(Reader, FEngine.Settings);
end;

procedure TCodeFormatterExpert.InternalSaveSettings(const ConfigurationKey: string; Settings: TGExpertsSettings);
var
  Writer: IConfigWriter;
begin
  Writer := TGxConfigWrapper.Create(Settings, ConfigurationKey);
  TCodeFormatterConfigHandler.WriteSettings(Writer, FEngine.Settings);
end;

function TCodeFormatterExpert.ShowDoneDialog(ShowDone: Boolean): boolean;
var
  DoneForm: TfmCodeFormatterDone;
begin
  Result := ShowDone;
  if not Result then
    exit;
  DoneForm := TfmCodeFormatterDone.Create(nil);
  try
    DoneForm.ShowModal;
    Result := not DoneForm.chk_DontShowAgain.Checked;
  finally
    DoneForm.Free;
  end;
end;

end.

