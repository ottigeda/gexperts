unit GX_EditExceptionNotification;

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
  GX_BaseForm,
  StdCtrls,
  ComCtrls,
  ExtCtrls;

type
  TExceptionNotificationAction = (enaDisabled, enaIgnore, enaBreak);

type
  TfmGxEditExceptionNotification = class(TfmBaseForm)
    l_Exception: TLabel;
    cmb_Exception: TComboBox;
    l_Message: TLabel;
    ed_Message: TEdit;
    l_Matches: TLabel;
    re_Test: TRichEdit;
    b_OK: TButton;
    b_Cancel: TButton;
    rg_Action: TRadioGroup;
  private
    procedure SetData(const _Exception: string; const _MessageRe: string;
      _Action: TExceptionNotificationAction);
    procedure GetData(out _Exception: string; out _MessageRe: string;
      out _Action: TExceptionNotificationAction);
  public
    class function Execute(_Owner: TWinControl; var _Exception: string; var _MessageRe: string;
      var _Action: TExceptionNotificationAction): Boolean;
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  u_dzVclUtils;

{ TfmGxEditExceptionNotification }

class function TfmGxEditExceptionNotification.Execute(_Owner: TWinControl; var _Exception,
  _MessageRe: string; var _Action: TExceptionNotificationAction): Boolean;
var
  frm: TfmGxEditExceptionNotification;
begin
  frm := TfmGxEditExceptionNotification.Create(_Owner);
  try
    frm.SetData(_Exception, _MessageRe, _Action);
    Result := (frm.ShowModal = mrOk);
    if Result then
      frm.GetData(_Exception, _MessageRe, _Action);
  finally
    FreeAndNil(frm);
  end;
end;

constructor TfmGxEditExceptionNotification.Create(_Owner: TComponent);
begin
  inherited;
  TControl_SetMinConstraints(self);
end;

procedure TfmGxEditExceptionNotification.GetData(out _Exception, _MessageRe: string;
  out _Action: TExceptionNotificationAction);
begin
  _Exception := cmb_Exception.Text;
  _MessageRe := ed_Message.Text;
  _Action := TExceptionNotificationAction(rg_Action.ItemIndex);
end;

procedure TfmGxEditExceptionNotification.SetData(const _Exception, _MessageRe: string;
  _Action: TExceptionNotificationAction);
begin
  cmb_Exception.Text := _Exception;
  ed_Message.Text := _MessageRe;
  rg_Action.ItemIndex := Ord(_Action);
end;

end.
