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
  Int: IInterface;
begin
  // This buys (me) some time with adapting forms for High DPI by temporarily turning off
  // High DPI awareness. Works only for forms that are shown modally and don't
  // call into the IDE before closing.
  // All this is only necessary for Delphi 11 and later.
  // It does nothing for older Delphi versions.
  Int := TemporarilyDisableHighDpi;
  frm := Self.Create(nil);
  try
    frm.TemporarilyDisableHighDpiInterface := Int;
    Int := nil;
    frm.lblMesssage.Caption := Format(DoneMessage, [ATodoCount]);
    frm.ShowModal;
    Result := frm.chkDontShowAgain.Checked;
  finally
    FreeAndNil(frm);
  end;
end;

end.

