unit GX_BackupNotFound;

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
  StdCtrls,
  GX_BaseForm;

type
  TfmBackupNotFound = class(TfmBaseForm)
    l_NotFound: TLabel;
    m_NotFound: TMemo;
    b_Ok: TButton;
    procedure b_OKClick(Sender: TObject);
  private
  public
    class procedure Execute(_Owner: TWinControl; _NotFound: TStrings);
  end;

var
  fmBackupNotFound: TfmBackupNotFound;

implementation

{$R *.dfm}

procedure TfmBackupNotFound.b_OKClick(Sender: TObject);
begin
  inherited;
  Close;
end;

class procedure TfmBackupNotFound.Execute(_Owner: TWinControl; _NotFound: TStrings);
var
  frm: TfmBackupNotFound;
begin
  frm := TfmBackupNotFound.Create(_Owner);
  try
    frm.m_NotFound.Lines := _NotFound;
    frm.ShowModal;
  finally
    FreeAndNil(frm);
  end;
end;

end.
