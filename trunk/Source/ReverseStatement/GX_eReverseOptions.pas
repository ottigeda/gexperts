unit GX_eReverseOptions;

{$I GX_CondDefine.inc}

interface

uses
  Windows, Classes, Controls, StdCtrls, ExtCtrls, Grids, Forms,
  GX_BaseForm,
  GX_ReverseStatementUtils;

type
  TfmReverseOptions = class(TfmBaseForm)
    lblFormat: TLabel;
    sgRules: TStringGrid;
    pnlButtons: TPanel;
    btnAdd: TButton;
    btnEdit: TButton;
    btnDelete: TButton;
    btnReset: TButton;
    lblError: TLabel;
    gbTest: TGroupBox;
    lblTestInput: TLabel;
    edTestInput: TEdit;
    lblTestResult: TLabel;
    edTestResult: TEdit;
    btnOK: TButton;
    btnCancel: TButton;
    tmrTest: TTimer;
    procedure btnAddClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure edTestInputChange(Sender: TObject);
    procedure tmrTestTimer(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure sgRulesDblClick(Sender: TObject);
  private
    FDefaultRules: TReversalRuleList;
    function ValidateRules: Boolean;
    procedure DeleteRow(_Row: Integer);
  protected
    procedure ArrangeControls; override;
  public
    constructor Create(_Owner: TComponent); override;
    destructor Destroy; override;
    class function Execute(_Owner: TWinControl; _Rules: TReversalRuleList;
      _DefaultRules: TReversalRuleList): Boolean;
    procedure SetRules(_Rules: TReversalRuleList);
    procedure GetRules(_Rules: TReversalRuleList);
    procedure SetDefaultRules(_Rules: TReversalRuleList);
  end;

implementation

uses
  SysUtils,
  Dialogs,
  u_dzVclUtils,
  w_dzDialog,
  GX_RegExpr,
  GX_eReverseRuleEdit;

{$R *.dfm}

{ TfmReverseOptions }

constructor TfmReverseOptions.Create(_Owner: TComponent);
begin
  inherited;
  TControl_SetMinConstraints(Self);

  sgRules.Cells[0, 0] := 'Match Pattern';
  sgRules.Cells[1, 0] := 'Replacement';

  InitDpiScaler;
end;

destructor TfmReverseOptions.Destroy;
begin
  FreeAndNil(FDefaultRules);
  inherited;
end;

class function TfmReverseOptions.Execute(_Owner: TWinControl; _Rules: TReversalRuleList;
  _DefaultRules: TReversalRuleList): Boolean;
var
  frm: TfmReverseOptions;
begin
  frm := TfmReverseOptions.Create(_Owner);
  try
    TForm_CenterOn(frm, _Owner);
    frm.SetRules(_Rules);
    frm.SetDefaultRules(_DefaultRules);
    Result := frm.ShowModal = mrOk;
    if Result then
      frm.GetRules(_Rules);
  finally
    FreeAndNil(frm);
  end;
end;

procedure TfmReverseOptions.SetRules(_Rules: TReversalRuleList);
var
  i: Integer;
begin
  if _Rules.Count = 0 then
  begin
    sgRules.RowCount := 2;
    sgRules.Cells[0, 1] := '';
    sgRules.Cells[1, 1] := '';
  end else
  begin
    sgRules.RowCount := _Rules.Count + 1;
    for i := 0 to _Rules.Count - 1 do
    begin
      sgRules.Cells[0, i + 1] := _Rules[i].MatchPattern;
      sgRules.Cells[1, i + 1] := _Rules[i].ReplaceTemplate;
    end;
  end;
  TGrid_Resize(sgRules, [roUseGridWidth]);
end;

procedure TfmReverseOptions.GetRules(_Rules: TReversalRuleList);
var
  i: Integer;
  Match, Replace: string;
begin
  _Rules.Clear;
  for i := 1 to sgRules.RowCount - 1 do
  begin
    Match := Trim(sgRules.Cells[0, i]);
    Replace := Trim(sgRules.Cells[1, i]);
    if (Match <> '') or (Replace <> '') then
      _Rules.Add(Match, Replace);
  end;
end;

procedure TfmReverseOptions.SetDefaultRules(_Rules: TReversalRuleList);
begin
  FreeAndNil(FDefaultRules);
  FDefaultRules := TReversalRuleList.Create;
  FDefaultRules.Assign(_Rules);
end;

procedure TfmReverseOptions.btnAddClick(Sender: TObject);
var
  Match, Replace: string;
  NewRow: Integer;
begin
  Match := '';
  Replace := '';
  if TfmReverseRuleEdit.Execute(Self, Match, Replace) then
  begin
    NewRow := sgRules.RowCount;
    sgRules.RowCount := NewRow + 1;
    sgRules.Cells[0, NewRow] := Match;
    sgRules.Cells[1, NewRow] := Replace;
    sgRules.Row := NewRow;
    TGrid_Resize(sgRules, [roUseGridWidth]);
  end;
end;

procedure TfmReverseOptions.btnEditClick(Sender: TObject);
var
  Row: Integer;
  Match, Replace: string;
begin
  Row := sgRules.Row;
  if Row < 1 then
    Exit;
  Match := sgRules.Cells[0, Row];
  Replace := sgRules.Cells[1, Row];
  if TfmReverseRuleEdit.Execute(Self, Match, Replace) then
  begin
    sgRules.Cells[0, Row] := Match;
    sgRules.Cells[1, Row] := Replace;
    TGrid_Resize(sgRules, [roUseGridWidth]);
  end;
end;

procedure TfmReverseOptions.sgRulesDblClick(Sender: TObject);
begin
  btnEditClick(Sender);
end;

procedure TfmReverseOptions.DeleteRow(_Row: Integer);
var
  i: Integer;
begin
  if sgRules.RowCount <= 2 then
  begin
    // Keep at least one data row, just clear it
    sgRules.Cells[0, 1] := '';
    sgRules.Cells[1, 1] := '';
    Exit;
  end;
  for i := _Row to sgRules.RowCount - 2 do
  begin
    sgRules.Cells[0, i] := sgRules.Cells[0, i + 1];
    sgRules.Cells[1, i] := sgRules.Cells[1, i + 1];
  end;
  sgRules.RowCount := sgRules.RowCount - 1;
end;

procedure TfmReverseOptions.btnDeleteClick(Sender: TObject);
begin
  if sgRules.Row >= 1 then
    DeleteRow(sgRules.Row);
end;

procedure TfmReverseOptions.btnResetClick(Sender: TObject);
begin
  if FDefaultRules <> nil then
    if Tf_dzDialog.ShowMessage(mtConfirmation, 'Reset all rules to defaults?', [dbeYes, dbeNo], Self) = mrYes then
      SetRules(FDefaultRules);
end;

function TfmReverseOptions.ValidateRules: Boolean;
var
  i: Integer;
  Match, Replace: string;
  GroupCount, MaxRef: Integer;
  re: TRegExpr;
begin
  Result := True;
  lblError.Caption := '';
  re := TRegExpr.Create;
  try
    re.ModifierI := True;
    for i := 1 to sgRules.RowCount - 1 do
    begin
      Match := Trim(sgRules.Cells[0, i]);
      Replace := Trim(sgRules.Cells[1, i]);
      if (Match = '') and (Replace = '') then
        Continue;
      if (Match <> '') and (Replace = '') then
      begin
        lblError.Caption := Format('Row %d: match pattern has no replacement', [i]);
        sgRules.Row := i;
        sgRules.Col := 1;
        sgRules.SetFocus;
        Result := False;
        Exit;
      end;
      if (Match = '') and (Replace <> '') then
      begin
        lblError.Caption := Format('Row %d: replacement has no match pattern', [i]);
        sgRules.Row := i;
        sgRules.Col := 0;
        sgRules.SetFocus;
        Result := False;
        Exit;
      end;
      try
        re.Expression := '^' + Match + '$';
        re.Compile;
      except
        on E: ERegExpr do
        begin
          lblError.Caption := Format('Row %d: %s', [i, E.Message]);
          sgRules.Row := i;
          sgRules.Col := 0;
          sgRules.SetFocus;
          Result := False;
          Exit;
        end;
      end;
      GroupCount := CountCaptureGroups(Match);
      MaxRef := MaxBackReference(Replace);
      if MaxRef > GroupCount then
      begin
        lblError.Caption := Format('Row %d: replacement references $%d but pattern only has %d capture group(s)', [i, MaxRef, GroupCount]);
        sgRules.Row := i;
        sgRules.Col := 1;
        sgRules.SetFocus;
        Result := False;
        Exit;
      end;
    end;
  finally
    FreeAndNil(re);
  end;
end;

procedure TfmReverseOptions.edTestInputChange(Sender: TObject);
begin
  TTimer_Restart(tmrTest);
end;

procedure TfmReverseOptions.tmrTestTimer(Sender: TObject);
var
  Rules: TReversalRuleList;
  TestStr: string;
begin
  tmrTest.Enabled := False;
  TestStr := edTestInput.Text;
  if TestStr = '' then
  begin
    edTestResult.Text := '';
    Exit;
  end;
  Rules := TReversalRuleList.Create;
  try
    GetRules(Rules);
    if ReverseFunctionPairStr(TestStr, Rules) then
      edTestResult.Text := TestStr
    else
      edTestResult.Text := '(no rule matched)';
  finally
    FreeAndNil(Rules);
  end;
end;

procedure TfmReverseOptions.btnOKClick(Sender: TObject);
begin
  if ValidateRules then
    ModalResult := mrOk;
end;

procedure TfmReverseOptions.ArrangeControls;
begin
  inherited;
  sgRules.DefaultRowHeight := FScaler.Calc(20);
  TGrid_Resize(sgRules, [roUseGridWidth]);
end;

procedure TfmReverseOptions.FormResize(Sender: TObject);
begin
  TGrid_Resize(sgRules, [roUseGridWidth]);
end;

end.
