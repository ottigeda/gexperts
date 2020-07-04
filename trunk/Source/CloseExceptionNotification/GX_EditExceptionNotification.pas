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
  ExtCtrls,
  SynRegExpr;

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
    tim_InputDelay: TTimer;
    procedure tim_InputDelayTimer(Sender: TObject);
    procedure ed_MessageChange(Sender: TObject);
  private
    FMessage: string;
    FMatchColor: TColor;
    FRegEx: TRegExpr;
    procedure SetData(const _Exception: string; const _MessageRe: string; const _Message: string;
      _Action: TExceptionNotificationAction);
    procedure GetData(out _Exception: string; out _MessageRe: string;
      out _Action: TExceptionNotificationAction);
    procedure UpdateMatches;
  public
    class function Execute(_Owner: TWinControl; const _Message: string;
      var _Exception: string; var _MessageRe: string; var _Action: TExceptionNotificationAction): Boolean;
    constructor Create(_Owner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

uses
  u_dzVclUtils;

{ TfmGxEditExceptionNotification }

class function TfmGxEditExceptionNotification.Execute(_Owner: TWinControl; const _Message: string;
  var _Exception: string; var _MessageRe: string; var _Action: TExceptionNotificationAction): Boolean;
var
  frm: TfmGxEditExceptionNotification;
begin
  frm := TfmGxEditExceptionNotification.Create(_Owner);
  try
    frm.SetData(_Exception, _MessageRe, _Message, _Action);
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
  FRegEx := TRegExpr.Create;
end;

destructor TfmGxEditExceptionNotification.Destroy;
begin
  FreeAndNil(FRegEx);
  inherited;
end;

procedure TfmGxEditExceptionNotification.ed_MessageChange(Sender: TObject);
begin
  inherited;
  tim_InputDelay.Enabled := True;
end;

procedure TfmGxEditExceptionNotification.GetData(out _Exception, _MessageRe: string;
  out _Action: TExceptionNotificationAction);
begin
  _Exception := cmb_Exception.Text;
  _MessageRe := ed_Message.Text;
  _Action := TExceptionNotificationAction(rg_Action.ItemIndex);
end;

procedure TfmGxEditExceptionNotification.SetData(const _Exception, _MessageRe, _Message: string;
  _Action: TExceptionNotificationAction);
begin
  cmb_Exception.Text := _Exception;
  ed_Message.Text := _MessageRe;
  FMessage := _Message;
  re_Test.Text := _Message;
  rg_Action.ItemIndex := Ord(_Action);
end;

procedure TfmGxEditExceptionNotification.tim_InputDelayTimer(Sender: TObject);
begin
  inherited;
  UpdateMatches;
end;

procedure TfmGxEditExceptionNotification.UpdateMatches;
var
  Res: Boolean;
  StartOfLine: Integer;
  LineIdx: Integer;
begin
  tim_InputDelay.Enabled := False;

  re_Test.Lines.BeginUpdate;
  try
    re_Test.Clear;
    LineIdx := re_Test.Lines.Add(FMessage);
    FRegEx.Expression := ed_Message.Text;
    try
      FRegEx.Compile;
      Res := FRegEx.Exec(FMessage);
      if Res then begin
        StartOfLine := re_Test.Perform(EM_LINEINDEX, LineIdx, 0);
        repeat
          re_Test.SelStart := StartOfLine + FRegEx.MatchPos[0] - 1;
          re_Test.SelLength := FRegEx.MatchLen[0];

          re_Test.SelAttributes.Color := FMatchColor;
          re_Test.SelAttributes.Style := [fsBold];
        until not FRegEx.ExecNext;
      end;
    except
      // ignore
    end;
    re_Test.SelStart := 0;
    re_Test.SelLength := 0;
  finally
    re_Test.Lines.EndUpdate;
  end;
end;

end.
