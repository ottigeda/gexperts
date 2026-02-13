unit GX_eReverseStatement;

interface

uses
  Classes, Controls,
  GX_ConfigurationInfo, GX_eSelectionEditorExpert,
  GX_ReverseStatementUtils;

type
  TReverseStatementExpert = class(TSelectionEditorExpert)
  private
    FMadeChanges: Boolean;
    FRules: TReversalRuleList;
    function ReverseOneLine(const S: string): string;
    function ReverseAssignment(var S: string): Boolean;
    function ReverseForLoop(var S: string): Boolean;
    function ReverseFunctionPair(var S: string): Boolean;
  protected
    function ProcessSelected(Lines: TStrings): Boolean; override;
    procedure InternalLoadSettings(_Settings: IExpertSettings); override;
    procedure InternalSaveSettings(_Settings: IExpertSettings); override;
  public
    class function GetName: string; override;
    constructor Create; override;
    destructor Destroy; override;
    procedure Configure(_Owner: TWinControl); override;
    function GetDefaultShortCut: TShortCut; override;
    function GetDisplayName: string; override;
    function GetHelpString: string; override;
    function HasConfigOptions: Boolean; override;
  end;

implementation

uses
  SysUtils, Forms,
  u_dzStringUtils, u_dzVclUtils,
  GX_EditorExpert, GX_OtaUtils, GX_GenericUtils,
  GX_eReverseOptions;

// Trailing-comment suffix used in patterns: optional whitespace + line comment
// TC = (\s*(?://.*|\{[^}]*\}|\(\*.*?\*\))?)

procedure LoadDefaultRules(_Rules: TReversalRuleList);

  procedure AddPair(_Rules: TReversalRuleList; const FuncA, FuncB: string);
  // Simple conversion pair: LHS := [Obj.]FuncA(Arg); <-> Arg := [Obj.]FuncB(LHS);
  // Arg is restricted to [\w.]+ (identifiers) since it becomes the new LHS after swap.
  const
    TC = '(\s*(?://.*|\{[^}]*\}|\(\*.*?\*\))?)';
  begin
    _Rules.Add(
      '(\s*)([\w.]+)\s*:=\s*([\w.]*?)' + FuncA + '\(([\w.]+)\)\s*;' + TC,
      '$1$4 := $3' + FuncB + '($2);$5');
    _Rules.Add(
      '(\s*)([\w.]+)\s*:=\s*([\w.]*?)' + FuncB + '\(([\w.]+)\)\s*;' + TC,
      '$1$4 := $3' + FuncA + '($2);$5');
  end;

  procedure AddReadWrite(_Rules: TReversalRuleList; const TypeName, DefaultValue: string);
  // Registry/INI Read/Write pair with 3 params
  const
    TC = '(\s*(?://.*|\{[^}]*\}|\(\*.*?\*\))?)';
  begin
    // Read -> Write: drop default param, insert LHS as 3rd param
    _Rules.Add(
      '(\s*)([\w.]+)\s*:=\s*([\w.]*?)Read' + TypeName + '\(\s*([^,()]+?)\s*,\s*([^,()]+?)\s*,\s*([^()]+?)\s*\)\s*;' + TC,
      '$1$3Write' + TypeName + '($4, $5, $2);$7');
    // Write -> Read: create assignment, add default
    _Rules.Add(
      '(\s*)([\w.]*?)Write' + TypeName + '\(\s*([^,()]+?)\s*,\s*([^,()]+?)\s*,\s*([\w.]+)\s*\)\s*;' + TC,
      '$1$5 := $2Read' + TypeName + '($3, $4, ' + DefaultValue + ');$6');
  end;

begin
  _Rules.Clear;
  // Simple conversion pairs
  AddPair(_Rules, 'IntToStr', 'StrToInt');
  AddPair(_Rules, 'FloatToStr', 'StrToFloat');
  AddPair(_Rules, 'BoolToStr', 'StrToBool');
  AddPair(_Rules, 'DateToStr', 'StrToDate');
  AddPair(_Rules, 'DateTimeToStr', 'StrToDateTime');
  AddPair(_Rules, 'TimeToStr', 'StrToTime');
  AddPair(_Rules, 'CurrToStr', 'StrToCurr');
  // Registry/INI Read/Write pairs
  AddReadWrite(_Rules, 'String', '''''');
  AddReadWrite(_Rules, 'Integer', '0');
  AddReadWrite(_Rules, 'Bool', 'False');
  AddReadWrite(_Rules, 'Float', '0');
  AddReadWrite(_Rules, 'Date', 'Date');
  AddReadWrite(_Rules, 'DateTime', 'Now');
  AddReadWrite(_Rules, 'Time', 'Time');
end;

{ TReverseStatementExpert }

constructor TReverseStatementExpert.Create;
begin
  inherited Create;
  FRules := TReversalRuleList.Create;
end;

destructor TReverseStatementExpert.Destroy;
begin
  FreeAndNil(FRules);
  inherited;
end;

procedure TReverseStatementExpert.Configure(_Owner: TWinControl);
var
  frm: TfmReverseOptions;
  TempLines: TStringList;
begin
  frm := TfmReverseOptions.Create(_Owner);
  try
    TForm_CenterOn(frm, _Owner);
    TempLines := TStringList.Create;
    try
      FRules.SaveToStrings(TempLines);
      frm.Memo.Lines.Assign(TempLines);
    finally
      FreeAndNil(TempLines);
    end;

    if frm.ShowModal = mrOk then
    begin
      TempLines := TStringList.Create;
      try
        TempLines.Assign(frm.Memo.Lines);
        FRules.LoadFromStrings(TempLines);
      finally
        FreeAndNil(TempLines);
      end;
      SaveSettings;
    end;
  finally
    FreeAndNil(frm);
  end;
end;

function TReverseStatementExpert.GetDefaultShortCut: TShortCut;
begin
  Result := scAlt + scShift + Ord('R');
end;

function TReverseStatementExpert.GetDisplayName: string;
resourcestring
  SReverseStatementName = 'Reverse Statement';
begin
  Result := SReverseStatementName;
end;

function TReverseStatementExpert.GetHelpString: string;
resourcestring
  SReverseStatementHelp =
    '  This expert reverses all assignment statements, the loop direction of ' +
    'for loops, and configured regex-based reversal rules in a selected block of code.  ' + sLineBreak +
    'For example, a statement like "Foo := Bar;" would be changed into: "Bar := Foo;"  ' +
    'and "for i := 0 to 99 do" into: "for i := 99 downto 0 do".' + sLineBreak +
    'Default rules handle function pairs like IntToStr/StrToInt and ' +
    'registry Read/Write methods. Use Configure to edit the regex rules.' + sLineBreak + sLineBreak +
    '  This expert supports both Delphi and C++ assignments, but only Delphi for loops. ' +
    'It expects all reversible statements to be contained on a single line.';
begin
  Result := SReverseStatementHelp;
end;

class function TReverseStatementExpert.GetName: string;
begin
  Result := 'ReverseStatement';
end;

function TReverseStatementExpert.HasConfigOptions: Boolean;
begin
  Result := True;
end;

procedure TReverseStatementExpert.InternalLoadSettings(_Settings: IExpertSettings);
var
  TempList: TStringList;
begin
  inherited InternalLoadSettings(_Settings);

  TempList := TStringList.Create;
  try
    _Settings.ReadStrings('ReversalRules', TempList, 'Line');
    if TempList.Count = 0 then
      LoadDefaultRules(FRules)
    else
      FRules.LoadFromStrings(TempList);
  finally
    FreeAndNil(TempList);
  end;
end;

procedure TReverseStatementExpert.InternalSaveSettings(_Settings: IExpertSettings);
var
  TempList: TStringList;
begin
  inherited InternalSaveSettings(_Settings);

  TempList := TStringList.Create;
  try
    FRules.SaveToStrings(TempList);
    _Settings.WriteStrings('ReversalRules', TempList, 'Line');
  finally
    FreeAndNil(TempList);
  end;
end;

function TReverseStatementExpert.ProcessSelected(Lines: TStrings): Boolean;
var
  i: Integer;
begin
  Assert(Assigned(Lines));
  FMadeChanges := False;
  for i := 0 to Lines.Count - 1 do
    Lines[i] := ReverseOneLine(Lines[i]);
  Result := FMadeChanges;
end;

function TReverseStatementExpert.ReverseOneLine(const S: string): string;
begin
  Result := S;
  if ReverseForLoop(Result) then
    Exit;
  if ReverseFunctionPair(Result) then
    Exit;
  if ReverseAssignment(Result) then
    Exit;
end;

function TReverseStatementExpert.ReverseFunctionPair(var S: string): Boolean;
begin
  Result := ReverseFunctionPairStr(S, FRules);
  if Result then
    FMadeChanges := True;
end;

function TReverseStatementExpert.ReverseAssignment(var S: string): Boolean;
begin
  Result := ReverseAssignmentStr(S,
    IsCPPSourceModule(Ota.GetCurrentSourceEditor.FileName));
  if Result then
    FMadeChanges := True;
end;

function TReverseStatementExpert.ReverseForLoop(var S: string): Boolean;
begin
  Result := ReverseForLoopStr(S);
  if Result then
    FMadeChanges := True;
end;

initialization
  RegisterEditorExpert(TReverseStatementExpert);
end.
