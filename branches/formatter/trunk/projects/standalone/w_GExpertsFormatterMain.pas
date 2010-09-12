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
  Forms,
  Dialogs,
  StdCtrls;

type
  Tf_GExpertsFormatterMain = class(TForm)
    ed_FileToFormat: TEdit;
    l_: TLabel;
    od_File: TOpenDialog;
    b_SelectFile: TButton;
    b_Format: TButton;
    b_Exit: TButton;
    procedure b_ExitClick(Sender: TObject);
    procedure ed_FileToFormatChange(Sender: TObject);
    procedure b_SelectFileClick(Sender: TObject);
    procedure b_FormatClick(Sender: TObject);
  private
  public
  end;

procedure Main;

implementation

{$R *.dfm}

uses
  GX_VerDepConst;

type
  TFormatFileFunc = function(_FileName: PChar): Boolean;
  TFormatFilesFunc = function(_FileNames: PChar): Boolean;

var
  FormatFile: TFormatFileFunc = nil;
  FormatFiles: TFormatFilesFunc = nil;
  HModule: THandle;

function LoadGExperts: boolean;
const
  EntryPoint_FormatFile = 'FormatFile';
  EntryPoint_FormatFiles = 'FormatFiles';
var
  p: Pointer;
begin
  Result := false;
  HModule := SafeLoadLibrary(GExpertsDll);
  if HModule = 0 then begin
    ShowMessageFmt('Could not load %s', [GExpertsDll]);
    exit;
  end;
  p := GetProcAddress(HModule, EntryPoint_FormatFile);
  if p = nil then begin
    ShowMessageFmt('%s does not export entry point %s', [GExpertsDll,
      EntryPoint_FormatFile]);
    exit;
  end;
  FormatFile := p;
  p := GetProcAddress(HModule, EntryPoint_FormatFiles);
  if p = nil then begin
    ShowMessageFmt('%s does not export entry point %s', [GExpertsDll,
      EntryPoint_FormatFiles]);
    exit;
  end;
  FormatFiles := p;
  Result := true;
end;

procedure Interactive;
var
  frm: Tf_GExpertsFormatterMain;
begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(Tf_GExpertsFormatterMain, frm);
  Application.Run;
end;

procedure doFormatFile(FileName: string);
begin
  FileName := ExpandFileName(FileName);
  FormatFile(PChar(FileName));
end;

procedure Batch;
var
  FileName: string;
  FileList: TStringList;
  i: Integer;
begin
  FileList := TStringList.Create;
  try
    for i := 1 to ParamCount do begin
      FileName := ParamStr(i);
      FileName := ExpandFileName(FileName);
      FileList.Add(FileName);
    end;
    FileList.StrictDelimiter := True;
    FileList.Delimiter := ';';
    FormatFiles(PChar(FileList.DelimitedText));
  finally
    FileList.Free;
  end;
end;

procedure Main;
begin
  if LoadGExperts then begin
    try
      if ParamCount = 0 then
        Interactive
      else
        Batch;
    finally
      FreeLibrary(HModule);
    end;
  end;
end;

procedure Tf_GExpertsFormatterMain.b_SelectFileClick(Sender: TObject);
begin
  if od_File.Execute then
    ed_FileToFormat.Text := od_File.FileName;
end;

procedure Tf_GExpertsFormatterMain.b_ExitClick(Sender: TObject);
begin
  Close;
end;

procedure Tf_GExpertsFormatterMain.b_FormatClick(Sender: TObject);
begin
  doFormatFile(ed_FileToFormat.Text);
end;

procedure Tf_GExpertsFormatterMain.ed_FileToFormatChange(Sender: TObject);
var
  fn: string;
begin
  fn := ed_FileToFormat.Text;
  b_Format.Enabled := FileExists(fn);
end;

end.

