unit GX_eReverseOptions;

{$I GX_CondDefine.inc}

interface

uses
  Windows, Classes, Controls, StdCtrls, ExtCtrls, Graphics, Forms,
  SynEdit,
  SynMemo,
  GX_BaseForm;

type
  TfmReverseOptions = class(TfmBaseForm)
    lblFormat: TLabel;
    pnlMemo: TPanel;
    lblError: TLabel;
    lblTestInput: TLabel;
    edTestInput: TEdit;
    btnTest: TButton;
    lblTestResult: TLabel;
    edTestResult: TEdit;
    btnOK: TButton;
    btnCancel: TButton;
    procedure btnTestClick(Sender: TObject);
  private
    FMemo: TSynMemo;
    FInvalidLines: array of Boolean;
    procedure MemoChange(Sender: TObject);
    procedure MemoSpecialLineColors(Sender: TObject; Line: Integer;
      var Special: Boolean; var FG, BG: TColor);
    procedure ValidateLines;
  public
    constructor Create(_Owner: TComponent); override;
    property Memo: TSynMemo read FMemo;
  end;

implementation

uses
  SysUtils,
  u_dzVclUtils,
  GX_EnhancedEditor,
  GX_RegExpr,
  GX_ReverseStatementUtils;

{$R *.dfm}

{ TfmReverseOptions }

constructor TfmReverseOptions.Create(_Owner: TComponent);
begin
  inherited;
  TControl_SetMinConstraints(Self);

  FMemo := TSynMemo.Create(Self);
  FMemo.Align := alClient;
  FMemo.Parent := pnlMemo;
  FMemo.Gutter.Visible := False;
  FMemo.Font.Height := -13;
  FMemo.Font.Name := 'Courier New';
  FMemo.ActiveLineColor := TGxEnhancedEditor.DefaultActiveLineColor;
  FMemo.ScrollBars := ssBoth;
  FMemo.WordWrap := False;
  FMemo.RightEdge := 0;
  FMemo.Options := FMemo.Options - [eoScrollPastEol, eoScrollPastEof];
  FMemo.OnChange := MemoChange;
  FMemo.OnSpecialLineColors := MemoSpecialLineColors;

  InitDpiScaler;
end;

procedure TfmReverseOptions.ValidateLines;
var
  i: Integer;
  Line: string;
  ContentLines: TStringList;
  re: TRegExpr;
  FirstError: string;
  ContentIdx: Integer;
  LineMap: array of Integer; // maps content line index to memo line index
begin
  SetLength(FInvalidLines, FMemo.Lines.Count);
  FirstError := '';
  for i := 0 to FMemo.Lines.Count - 1 do
    FInvalidLines[i] := False;

  // Collect non-comment, non-empty lines and track their original line numbers
  ContentLines := TStringList.Create;
  try
    SetLength(LineMap, 0);
    for i := 0 to FMemo.Lines.Count - 1 do
    begin
      Line := Trim(FMemo.Lines[i]);
      if (Line = '') or (Line[1] = '#') then
        Continue;
      SetLength(LineMap, Length(LineMap) + 1);
      LineMap[High(LineMap)] := i;
      ContentLines.Add(Line);
    end;

    // Validate match patterns (even-indexed content lines: 0, 2, 4, ...)
    re := TRegExpr.Create;
    try
      ContentIdx := 0;
      while ContentIdx < ContentLines.Count do
      begin
        try
          re.Expression := '^' + ContentLines[ContentIdx] + '$';
          re.ModifierI := True;
          re.Compile;
        except
          on E: ERegExpr do
          begin
            FInvalidLines[LineMap[ContentIdx]] := True;
            if FirstError = '' then
              FirstError := Format('Line %d: %s', [LineMap[ContentIdx] + 1, E.Message]);
          end;
        end;
        Inc(ContentIdx, 2); // skip replacement line
      end;
    finally
      FreeAndNil(re);
    end;

    // Check for unpaired trailing line
    if (ContentLines.Count > 0) and Odd(ContentLines.Count) then
    begin
      i := LineMap[ContentLines.Count - 1];
      FInvalidLines[i] := True;
      if FirstError = '' then
        FirstError := Format('Line %d: unpaired match pattern (missing replacement line)', [i + 1]);
    end;
  finally
    FreeAndNil(ContentLines);
  end;
  lblError.Caption := FirstError;
end;

procedure TfmReverseOptions.MemoChange(Sender: TObject);
begin
  ValidateLines;
  FMemo.Invalidate;
end;

procedure TfmReverseOptions.MemoSpecialLineColors(Sender: TObject; Line: Integer;
  var Special: Boolean; var FG, BG: TColor);
begin
  if (Line >= 1) and (Line <= Length(FInvalidLines)) and FInvalidLines[Line - 1] then
  begin
    Special := True;
    FG := clRed;
  end;
end;

procedure TfmReverseOptions.btnTestClick(Sender: TObject);
var
  Rules: TReversalRuleList;
  TempLines: TStringList;
  TestStr: string;
begin
  Rules := TReversalRuleList.Create;
  TempLines := TStringList.Create;
  try
    TempLines.Assign(FMemo.Lines);
    Rules.LoadFromStrings(TempLines);
    TestStr := edTestInput.Text;
    if ReverseFunctionPairStr(TestStr, Rules) then
      edTestResult.Text := TestStr
    else
      edTestResult.Text := '(no rule matched)';
  finally
    FreeAndNil(TempLines);
    FreeAndNil(Rules);
  end;
end;

end.
