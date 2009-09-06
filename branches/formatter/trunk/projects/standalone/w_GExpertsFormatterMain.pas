unit w_GExpertsFormatterMain;

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
var
  FormatFile: TFormatFileFunc = nil;
  HModule: THandle;

function LoadGExperts: boolean;
const
  EntryPoint = 'FormatFile';
var
  p: Pointer;
begin
  Result := false;
  HModule := SafeLoadLibrary(GExpertsDll);
  if HModule = 0 then begin
    ShowMessageFmt('Could not load %s', [GExpertsDll]);
    exit;
  end;

  p := GetProcAddress(HModule, EntryPoint);
  if p = nil then begin
    ShowMessageFmt('%s does not export entry point %s', [GExpertsDll,
      EntryPoint]);
    exit;
  end;
  FormatFile := p;

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
begin
  doFormatFile(ParamStr(1));
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

