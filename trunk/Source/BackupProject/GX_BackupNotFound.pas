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
    l_Note: TLabel;
    procedure b_OKClick(Sender: TObject);
  private
  public
    class procedure Execute(_Owner: TWinControl; _NotFound: TStrings; _FollowLibraryPath: Boolean);
  end;

var
  fmBackupNotFound: TfmBackupNotFound;

implementation

uses
  u_dzVclUtils;

{$R *.dfm}

procedure TfmBackupNotFound.b_OKClick(Sender: TObject);
begin
  inherited;
  Close;
end;

class procedure TfmBackupNotFound.Execute(_Owner: TWinControl; _NotFound: TStrings; _FollowLibraryPath: Boolean);
var
  frm: TfmBackupNotFound;
  Int: IInterface;
begin
  // This buys (me) some time with adapting forms for High DPI by temporarily turning off
  // High DPI awareness. Works only for forms that are shown modally and don't
  // call into the IDE before closing.
  // All this is only necessary for Delphi 11 and later.
  // It does nothing for older Delphi versions.
  int := TemporarilyDisableHighDpi;
  frm := TfmBackupNotFound.Create(_Owner);
  try
    frm.TemporarilyDisableHighDpiInterface := int;
    Int := nil;
    TForm_CenterOn(frm, _Owner);
    TControl_SetMinConstraints(frm);
    frm.m_NotFound.Lines := _NotFound;
    frm.l_NotFound.Visible := not _FollowLibraryPath;
    frm.ShowModal;
  finally
    FreeAndNil(frm);
  end;
end;

end.
