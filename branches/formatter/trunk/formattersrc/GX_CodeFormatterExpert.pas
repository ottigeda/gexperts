// the actual code formatter expert, called either from GX_Formatter or GX_eCodeFormatter
// Original Author:     Thomas Mueller (http://www.dummzeuch.de)
unit GX_CodeFormatterExpert;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils,
  GX_ConfigurationInfo,
  GX_CodeFormatterEngine;

type
  TCodeFormatterExpert = class
  private
    FEngine: TCodeFormatterEngine;
    function ShowDoneDialog(ShowDone: Boolean): boolean;
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
  Classes,
  ToolsApi,
  GX_OtaUtils,
  GX_GenericUtils,
  GX_CodeFormatterGXConfigWrapper,
  GX_CodeFormatterTypes,
  GX_CodeFormatterConfig,
  GX_CodeFormatterBookmarks,
  GX_CodeFormatterBreakpoints,
  GX_CodeFormatterDefaultSettings,
  GX_CodeFormatterConfigHandler,
  GX_CodeFormatterDone,
  GX_CodeFormatterSettings;

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
  FirstLine: string;
begin
  SourceEditor := GxOtaGetCurrentSourceEditor;
  if not Assigned(SourceEditor) then
    raise ECodeFormatter.Create(str_NoEditor);
  FileName := SourceEditor.FileName;
  if not (IsPascalSourceFile(FileName) or IsDelphiPackage(FileName) or FileMatchesExtension(FileName, '.tpl')) then
    raise ECodeFormatter.CreateFmt(str_UnsupportedFileTypeS, [ExtractFileName(FileName)]);

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
      FirstLine := FullText[0];
      if SameText(Copy(FirstLine, 1, 20), '{GXFormatter.config=') then begin
        FirstLine := Trim(Copy(FirstLine, 21, Length(FirstLine) - 21));
        TempSettings := TCodeFormatterSettings.Create;
        if TCodeFormatterConfigHandler.GetDefaultConfig(FirstLine, TempSettings) then begin
          OrigSettings := FEngine.Settings.Settings;
          FEngine.Settings.Settings := TempSettings.Settings;
        end else
          FreeAndNil(TempSettings);
      end;

      if FEngine.Execute(FullText) then begin
        GxOtaReplaceEditorText(SourceEditor, FullText.Text);
        Breakpoints.RestoreItems;
        Bookmarks.RestoreItems;
        for i := 0 to SourceEditor.EditViewCount - 1 do
          SourceEditor.EditViews[i].Paint;
        FEngine.Settings.ShowDoneDialog := ShowDoneDialog(FEngine.Settings.ShowDoneDialog);
      end;
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

