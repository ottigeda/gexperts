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
  Dialogs;

type
  Tf_GExpertsFormatterMain = class(TForm)
  private
  public
  end;

procedure Main;

implementation

{$R *.dfm}

uses
  GX_VerDepConst;

type
  TFormatFileFunc = function(_FileName: PChar): Boolean; stdcall;
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
    ShowMessageFmt('%s does not export entry point %s', [GExpertsDll, EntryPoint]);
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

procedure Batch;
var
  FileName: string;
begin
  FileName := ParamStr(1);
  FileName := ExpandFileName(FileName);
  FormatFile(PChar(FileName));
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

end.

