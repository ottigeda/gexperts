unit w_GExpertsFormatterMain;

// Calls GExperts dll for formatting
// stand alone version by Ulrich Gerhardt

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  ComCtrls,
  Forms,
  ShellApi,
  Dialogs,
  StdCtrls,
  w_GExpertsFormatterAbout;

type
  Tf_GExpertsFormatterMain = class(TForm)
    l_FilesToFormat: TLabel;
    od_File: TOpenDialog;
    b_SelectFile: TButton;
    b_Format: TButton;
    b_Exit: TButton;
    b_Settings: TButton;
    b_About: TButton;
    TheStatusBar: TStatusBar;
    m_FilesToFormat: TMemo;
    procedure b_ExitClick(Sender: TObject);
    procedure m_FilesToFormatChange(Sender: TObject);
    procedure b_SelectFileClick(Sender: TObject);
    procedure b_FormatClick(Sender: TObject);
    procedure b_SettingsClick(Sender: TObject);
    procedure b_AboutClick(Sender: TObject);
  private
    procedure HandleFilesDropped(_Sender: TObject; _Files: TStrings);
  public
    constructor Create(_Onwer: TComponent); override;
    destructor Destroy; override;
  end;

procedure Main;

implementation

{$R *.dfm}

uses
  IniFiles,
  u_dzVclUtils,
  u_dzFileUtils,
  GX_CodeFormatterFormatter,
  GX_CodeFormatterEngine,
  GX_StringList,
  GX_CodeFormatterConfigHandler,
  GX_GenericUtils,
  GX_CodeFormatterConfig,
  GX_CodeFormatterGXConfigWrapper;

procedure Interactive;
var
  frm: Tf_GExpertsFormatterMain;
begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(Tf_GExpertsFormatterMain, frm);
  Application.Run;
end;

function CreateAndConfigureEngine: TCodeFormatterEngine;
var
  Ini: TCustomIniFile;
  ConfigReader: IConfigReader;
begin
  Ini := nil;
  try
    Result := TCodeFormatterEngine.Create;
    Ini := TMemIniFile.Create(GetConfigFilename);
    ConfigReader := TIniFileWrapper.Create(Ini);
    TCodeFormatterConfigHandler.ReadSettings(ConfigReader, Result.Settings,
      GetCapitalizationFilename);
  finally
    FreeAndNil(Ini);
  end;
end;

function doFormatFile(_Engine: TCodeFormatterEngine; _FileName: string): Boolean;
var
  Source: TGXUnicodeStringList;
begin
  _FileName := ExpandFileName(_FileName);
  Source := TGXUnicodeStringList.Create;
  try
    Source.LoadFromFile(_FileName);
    Result := _Engine.Execute(Source);
    if Result then begin
      Source.SaveToFile(_FileName);
    end;
  finally
    FreeAndNil(Source);
  end;
end;

function FormatFile(const _FileName: string): Boolean;
var
  Engine: TCodeFormatterEngine;
begin
  Engine := CreateAndConfigureEngine;
  try
    Result := doFormatFile(Engine, _FileName);
  finally
    FreeAndNil(Engine);
  end;
end;

procedure Batch;
var
  Engine: TCodeFormatterEngine;
  fn: string;
  i: Integer;
begin
  Engine := CreateAndConfigureEngine;
  try
    for i := 1 to ParamCount do begin
      fn := ParamStr(i);
      doFormatFile(Engine, fn);
    end;
  finally
    FreeAndNil(Engine);
  end;
end;

procedure Main;
begin
  if ParamCount = 0 then begin
    Interactive;
  end else begin
    Batch;
  end;
end;

constructor Tf_GExpertsFormatterMain.Create(_Onwer: TComponent);
begin
  inherited;
  TWinControl_ActivateDropFiles(Self, HandleFilesDropped)
end;

destructor Tf_GExpertsFormatterMain.Destroy;
begin
  inherited;
end;

procedure Tf_GExpertsFormatterMain.HandleFilesDropped(_Sender: TObject; _Files: TStrings);
var
  i: Integer;
  Lines: TStrings;
begin
  Lines := m_FilesToFormat.Lines;
  Lines.BeginUpdate;
  try
    for i := 0 to _Files.Count - 1 do
      Lines.Add(_Files[i]);
  finally
    Lines.EndUpdate;
  end;
end;

procedure Tf_GExpertsFormatterMain.b_SelectFileClick(Sender: TObject);
begin
  if od_File.Execute then
    m_FilesToFormat.Lines.Add(od_File.FileName);
end;

procedure Tf_GExpertsFormatterMain.b_SettingsClick(Sender: TObject);
var
  Engine: TCodeFormatterEngine;
  Ini: TMemIniFile;
  ConfigWriter: IConfigWriter;
begin
  Engine := CreateAndConfigureEngine;
  try
    if TfmCodeFormatterConfig.Execute(Self, Engine.Settings) = mrOk then begin
      Ini := TMemIniFile.Create(GetConfigFilename);
      ConfigWriter := TIniFileWrapper.Create(Ini);
      TCodeFormatterConfigHandler.WriteSettings(ConfigWriter, Engine.Settings);
      Ini.UpdateFile;
    end;
  finally
    FreeAndNil(Engine);
  end;
end;

procedure Tf_GExpertsFormatterMain.b_AboutClick(Sender: TObject);
begin
  Tf_GExpertsFormatterAbout.Execute(Self, nil);
end;

procedure Tf_GExpertsFormatterMain.b_ExitClick(Sender: TObject);
begin
  Close;
end;

procedure Tf_GExpertsFormatterMain.b_FormatClick(Sender: TObject);
var
  fn: string;
  i: Integer;
  Formatted: Integer;
begin
  Formatted := 0;
  for i := 0 to m_FilesToFormat.Lines.Count do begin
    fn := m_FilesToFormat.Lines[i];
    if fn <> '' then begin
      TStatusBar_SetLongSimpleText(TheStatusBar, 'Formatting file ...');
      Application.ProcessMessages;
      if FormatFile(fn) then begin
        Inc(Formatted);
        TStatusBar_SetLongSimpleText(TheStatusBar, 'File formatted successfully.');
      end else begin
        TStatusBar_SetLongSimpleText(TheStatusBar, 'File remained unchanged.');
      end;
      Application.ProcessMessages;
    end;
  end;
  TStatusBar_SetLongSimpleText(TheStatusBar,
    Format('%d files have been changed during formatting..', [Formatted]));
end;

procedure Tf_GExpertsFormatterMain.m_FilesToFormatChange(Sender: TObject);
var
  fn: string;
  i: Integer;
  Lines: TStrings;
begin
  Lines := m_FilesToFormat.Lines;
  for i := 0 to Lines.Count - 1 do begin
    fn := m_FilesToFormat.Lines[i];
    if not FileExists(fn) then begin
      TStatusBar_SetLongSimpleText(TheStatusBar, 'File does not exist: ' + fn);
      b_Format.Enabled := False;
      Exit; //==>
    end;
  end;
  b_Format.Enabled := (Lines.Count > 0);
end;

end.

