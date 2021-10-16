unit GX_AutoTodoDone;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  GX_BaseForm;

type
  TfmAutoTodoDone = class(TfmBaseForm)
    lblMesssage: TLabel;
    btnOK: TButton;
    chkDontShowAgain: TCheckBox;
  public
    class function Execute(ATodoCount: Integer): Boolean;
  end;

implementation

{$R *.dfm}

uses
  u_dzVclUtils;

{ TfmAutoTodoDone }

class function TfmAutoTodoDone.Execute(ATodoCount: Integer): Boolean;
resourcestring
  DoneMessage = '%d comments have been inserted in empty code blocks.';
var
  frm: TfmAutoTodoDone;
begin
  frm := Self.Create(nil);
  try
    frm.lblMesssage.Caption := Format(DoneMessage, [ATodoCount]);
    frm.InitDpiScaler;
    frm.ShowModal;
    Result := frm.chkDontShowAgain.Checked;
  finally
    FreeAndNil(frm);
  end;
end;

end.

