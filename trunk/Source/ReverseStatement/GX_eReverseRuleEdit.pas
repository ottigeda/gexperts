unit GX_eReverseRuleEdit;

{$I GX_CondDefine.inc}

interface

uses
  Windows, Classes, Controls, StdCtrls, ExtCtrls, Forms,
  GX_BaseForm,
  u_dzDpiScaleUtils;

type
  TfmReverseRuleEdit = class(TfmBaseForm)
    lblMatch: TLabel;
    edMatch: TEdit;
    lblReplace: TLabel;
    edReplace: TEdit;
    lblError: TLabel;
    gbTest: TGroupBox;
    lblTestInput: TLabel;
    edTestInput: TEdit;
    lblTestResult: TLabel;
    edTestResult: TEdit;
    btnOK: TButton;
    btnCancel: TButton;
    tmrValidate: TTimer;
    procedure edMatchChange(Sender: TObject);
    procedure edReplaceChange(Sender: TObject);
    procedure edTestInputChange(Sender: TObject);
    procedure tmrValidateTimer(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    FDesignClientWidth: Integer;
    FDesignClientHeight: Integer;
    procedure ValidateAndTest;
  protected
    procedure ApplyDpi(_NewDpi: Integer; _NewBounds: PRect); override;
  public
    constructor Create(_Owner: TComponent); override;
    class function Execute(_Owner: TWinControl; var _Match, _Replace: string): Boolean;
  end;

implementation

uses
  SysUtils,
  u_dzVclUtils,
  GX_RegExpr,
  GX_ReverseStatementUtils;

{$R *.dfm}

{ TfmReverseRuleEdit }

constructor TfmReverseRuleEdit.Create(_Owner: TComponent);
begin
  inherited;
  FDesignClientWidth := ClientWidth;
  FDesignClientHeight := ClientHeight;
  InitDpiScaler;
end;

procedure TfmReverseRuleEdit.ApplyDpi(_NewDpi: Integer; _NewBounds: PRect);
var
  NewBounds: TRect;
  ScaledCW, ScaledCH: Integer;
begin
  // bsDialog forms cannot be resized by the user, and TFormDpiScaler.ApplyDpi
  // no longer scales the form size (the code was disabled because it broke
  // saving/restoring of sizable dialog sizes). For fixed-size dialogs we must
  // compute the scaled bounds ourselves so the form grows with DPI.
  if (_NewBounds = nil) and Assigned(FScaler) then
  begin
    ScaledCW := FScaler.Calc(FDesignClientWidth);
    ScaledCH := FScaler.Calc(FDesignClientHeight);
    NewBounds := BoundsRect;
    NewBounds.Right := NewBounds.Left + (Width - ClientWidth) + ScaledCW;
    NewBounds.Bottom := NewBounds.Top + (Height - ClientHeight) + ScaledCH;
    inherited ApplyDpi(_NewDpi, @NewBounds);
  end else
    inherited ApplyDpi(_NewDpi, _NewBounds);
end;

class function TfmReverseRuleEdit.Execute(_Owner: TWinControl; var _Match, _Replace: string): Boolean;
var
  frm: TfmReverseRuleEdit;
begin
  frm := TfmReverseRuleEdit.Create(_Owner);
  try
    TForm_CenterOn(frm, _Owner);
    frm.edMatch.Text := _Match;
    frm.edReplace.Text := _Replace;
    frm.ValidateAndTest;
    Result := frm.ShowModal = mrOk;
    if Result then
    begin
      _Match := Trim(frm.edMatch.Text);
      _Replace := Trim(frm.edReplace.Text);
    end;
  finally
    FreeAndNil(frm);
  end;
end;

procedure TfmReverseRuleEdit.edMatchChange(Sender: TObject);
begin
  TTimer_Restart(tmrValidate);
end;

procedure TfmReverseRuleEdit.edReplaceChange(Sender: TObject);
begin
  TTimer_Restart(tmrValidate);
end;

procedure TfmReverseRuleEdit.edTestInputChange(Sender: TObject);
begin
  TTimer_Restart(tmrValidate);
end;

procedure TfmReverseRuleEdit.tmrValidateTimer(Sender: TObject);
begin
  tmrValidate.Enabled := False;
  ValidateAndTest;
end;

procedure TfmReverseRuleEdit.ValidateAndTest;
var
  Match, Replace, TestStr: string;
  GroupCount, MaxRef: Integer;
  re: TRegExpr;
  Rules: TReversalRuleList;
begin
  lblError.Caption := '';
  edTestResult.Text := '';
  Match := Trim(edMatch.Text);
  Replace := Trim(edReplace.Text);

  if (Match = '') or (Replace = '') then
  begin
    btnOK.Enabled := False;
    if (Match = '') and (Replace = '') then
      lblError.Caption := ''
    else if Match = '' then
      lblError.Caption := 'Match pattern is required'
    else
      lblError.Caption := 'Replacement is required';
    Exit;
  end;

  re := TRegExpr.Create;
  try
    re.ModifierI := True;
    try
      re.Expression := '^' + Match + '$';
      re.Compile;
    except
      on E: ERegExpr do
      begin
        lblError.Caption := E.Message;
        btnOK.Enabled := False;
        Exit;
      end;
    end;
  finally
    FreeAndNil(re);
  end;

  GroupCount := CountCaptureGroups(Match);
  MaxRef := MaxBackReference(Replace);
  if MaxRef > GroupCount then
  begin
    lblError.Caption := Format('Replacement references $%d but pattern only has %d capture group(s)',
      [MaxRef, GroupCount]);
    btnOK.Enabled := False;
    Exit;
  end;

  btnOK.Enabled := True;

  TestStr := edTestInput.Text;
  if TestStr <> '' then
  begin
    Rules := TReversalRuleList.Create;
    try
      Rules.Add(Match, Replace);
      if ReverseFunctionPairStr(TestStr, Rules) then
        edTestResult.Text := TestStr
      else
        edTestResult.Text := '(no match)';
    finally
      FreeAndNil(Rules);
    end;
  end;
end;

procedure TfmReverseRuleEdit.btnOKClick(Sender: TObject);
begin
  ValidateAndTest;
  if btnOK.Enabled then
    ModalResult := mrOk;
end;

end.
