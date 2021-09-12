unit GX_FilterExceptionsEdit;

{$I GX_CondDefine.inc}

{$IFNDEF GX_DELPHI2005_UP}
'This only works for Delphi 2005 and newer'
{$ENDIF}

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
  TExceptionFilterAction = (efaIgnore, efaBreak, efaDisabled);

type
  TfmGxFilterExceptionsEdit = class(TfmBaseForm)
    b_OK: TButton;
    b_Cancel: TButton;
    rg_Action: TRadioGroup;
    tim_InputDelay: TTimer;
    grp_Project: TGroupBox;
    ed_Project: TEdit;
    b_ProjectName: TButton;
    b_ProjectSession: TButton;
    b_ProjectAny: TButton;
    grp_ExceptionClass: TGroupBox;
    cmb_Exception: TComboBox;
    b_ExceptionCurrent: TButton;
    grp_Message: TGroupBox;
    l_Matches: TLabel;
    ed_Message: TEdit;
    re_Test: TRichEdit;
    b_AllInCurrentSession: TButton;
    procedure tim_InputDelayTimer(Sender: TObject);
    procedure ed_MessageChange(Sender: TObject);
    procedure b_ProjectAnyClick(Sender: TObject);
    procedure b_ProjectSessionClick(Sender: TObject);
    procedure b_ProjectNameClick(Sender: TObject);
    procedure b_AllInCurrentSessionClick(Sender: TObject);
    procedure re_TestChange(Sender: TObject);
    procedure b_ExceptionCurrentClick(Sender: TObject);
  private
    FProject: string;
    FMessage: string;
    FMatchColor: TColor;
    FRegEx: TRegExpr;
    procedure SetData(const _Message: string; const _Project, _ExceptionClass, _MessageRe: string;
      _Action: TExceptionFilterAction);
    procedure GetData(out _Project, _ExceptionClass, _MessageRe: string;
      out _Action: TExceptionFilterAction);
    procedure UpdateMatches;
  public
    class function Execute(_Owner: TWinControl; const _Message: string;
      var _ProjectRe, _ExceptionClassRe, _MessageRe: string; var _Action: TExceptionFilterAction): Boolean;
    constructor Create(_Owner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

uses
  u_dzVclUtils;

{ TfmGxEditExceptionNotification }

class function TfmGxFilterExceptionsEdit.Execute(_Owner: TWinControl; const _Message: string;
  var _ProjectRe, _ExceptionClassRe, _MessageRe: string; var _Action: TExceptionFilterAction): Boolean;
var
  frm: TfmGxFilterExceptionsEdit;
  Int: IInterface;
begin
  // This buys (me) some time with adapting forms for High DPI by temporarily turning off
  // High DPI awareness. Works only for forms that are shown modally and don't
  // call into the IDE before closing.
  // All this is only necessary for Delphi 11 and later.
  // It does nothing for older Delphi versions.
  int := TemporarilyDisableHighDpi;
  frm := TfmGxFilterExceptionsEdit.Create(_Owner);
  try
    frm.TemporarilyDisableHighDpiInterface := int;
    Int := nil;
    frm.SetData(_Message, _ProjectRe, _ExceptionClassRe, _MessageRe, _Action);
    Result := (frm.ShowModal = mrOk);
    if Result then
      frm.GetData(_ProjectRe, _ExceptionClassRe, _MessageRe, _Action);
  finally
    FreeAndNil(frm);
  end;
end;

constructor TfmGxFilterExceptionsEdit.Create(_Owner: TComponent);
begin
  inherited;
  TControl_SetMinConstraints(self);
  FRegEx := TRegExpr.Create;
end;

destructor TfmGxFilterExceptionsEdit.Destroy;
begin
  FreeAndNil(FRegEx);
  inherited;
end;

procedure TfmGxFilterExceptionsEdit.b_ProjectNameClick(Sender: TObject);
begin
  ed_Project.Text := FProject;
end;

procedure TfmGxFilterExceptionsEdit.b_ProjectSessionClick(Sender: TObject);
begin
  ed_Project.Text := '';
end;

procedure TfmGxFilterExceptionsEdit.b_AllInCurrentSessionClick(Sender: TObject);
begin
  ed_Project.Text := '';
  ed_Message.Text := '';
  cmb_Exception.Text := '';
  ModalResult := mrOk;
end;

procedure TfmGxFilterExceptionsEdit.b_ExceptionCurrentClick(Sender: TObject);
begin
  cmb_Exception.Text := b_ExceptionCurrent.Caption;
end;

procedure TfmGxFilterExceptionsEdit.b_ProjectAnyClick(Sender: TObject);
begin
  ed_Project.Text := '.*';
end;

procedure TfmGxFilterExceptionsEdit.ed_MessageChange(Sender: TObject);
begin
  inherited;
  tim_InputDelay.Enabled := True;
end;

procedure TfmGxFilterExceptionsEdit.GetData(out _Project, _ExceptionClass, _MessageRe: string;
  out _Action: TExceptionFilterAction);
begin
  _Project := ed_Project.Text;
  _ExceptionClass := cmb_Exception.Text;
  _MessageRe := ed_Message.Text;
  _Action := TExceptionFilterAction(rg_Action.ItemIndex);
end;

procedure TfmGxFilterExceptionsEdit.re_TestChange(Sender: TObject);
begin
  tim_InputDelay.Enabled := True;
end;

procedure TfmGxFilterExceptionsEdit.SetData(const _Message: string;
  const _Project, _ExceptionClass, _MessageRe: string; _Action: TExceptionFilterAction);
begin
  FProject := _Project;
  if (_Project = '') or (_Project = '.*') then
    b_ProjectName.Visible := False
  else
    b_ProjectName.Caption := _Project;
  ed_Project.Text := _Project;
  cmb_Exception.Text := QuoteRegExprMetaChars(_ExceptionClass);

  b_ExceptionCurrent.Caption := _ExceptionClass;
  if _MessageRe = '' then
    ed_Message.Text := QuoteRegExprMetaChars(_Message)
  else
    ed_Message.Text := _MessageRe;
  if _Message <> '' then begin
    FMessage := _Message;
    re_Test.Text := _Message;
  end else begin
    FMessage := re_Test.Text;
  end;
  rg_Action.ItemIndex := Ord(_Action);
end;

procedure TfmGxFilterExceptionsEdit.tim_InputDelayTimer(Sender: TObject);
begin
  UpdateMatches;
end;

procedure TfmGxFilterExceptionsEdit.UpdateMatches;
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
    end; // FI:W501 Empty EXCEPT block
    re_Test.SelStart := 0;
    re_Test.SelLength := 0;
  finally
    re_Test.Lines.EndUpdate;
  end;
end;

end.
