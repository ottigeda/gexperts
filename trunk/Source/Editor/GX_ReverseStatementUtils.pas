unit GX_ReverseStatementUtils;

interface

uses
  Classes;

type
  TReversalRule = record
    MatchPattern: string;
    ReplaceTemplate: string;
  end;

  TReversalRuleList = class
  private
    FRules: array of TReversalRule;
    FCount: Integer;
    function GetRule(Index: Integer): TReversalRule;
  public
    procedure Clear;
    procedure Add(const AMatchPattern, AReplaceTemplate: string);
    procedure LoadFromStrings(Lines: TStrings);
    procedure SaveToStrings(Lines: TStrings);
    property Count: Integer read FCount;
    property Rules[Index: Integer]: TReversalRule read GetRule; default;
  end;

function ReverseForLoopStr(var S: string): Boolean;
function ReverseFunctionPairStr(var S: string; _Rules: TReversalRuleList): Boolean;
function ReverseAssignmentStr(var S: string; _IsCpp: Boolean): Boolean;

implementation

uses
  SysUtils,
  u_dzStringUtils,
  GX_GenericUtils,
  GX_RegExpr;

{ TReversalRuleList }

procedure TReversalRuleList.Clear;
begin
  SetLength(FRules, 0);
  FCount := 0;
end;

procedure TReversalRuleList.Add(const AMatchPattern, AReplaceTemplate: string);
begin
  if FCount >= Length(FRules) then
    SetLength(FRules, FCount + 16);
  FRules[FCount].MatchPattern := AMatchPattern;
  FRules[FCount].ReplaceTemplate := AReplaceTemplate;
  Inc(FCount);
end;

function TReversalRuleList.GetRule(Index: Integer): TReversalRule;
begin
  Result := FRules[Index];
end;

procedure TReversalRuleList.LoadFromStrings(Lines: TStrings);
var
  i: Integer;
  Line: string;
  ContentLines: TStringList;
begin
  Clear;
  ContentLines := TStringList.Create;
  try
    for i := 0 to Lines.Count - 1 do
    begin
      Line := Trim(Lines[i]);
      if (Line = '') or (Line[1] = '#') then
        Continue;
      ContentLines.Add(Line);
    end;
    // Pair them: 0+1, 2+3, etc.
    i := 0;
    while i + 1 < ContentLines.Count do
    begin
      Add(ContentLines[i], ContentLines[i + 1]);
      Inc(i, 2);
    end;
  finally
    FreeAndNil(ContentLines);
  end;
end;

procedure TReversalRuleList.SaveToStrings(Lines: TStrings);
var
  i: Integer;
begin
  Lines.Clear;
  for i := 0 to FCount - 1 do
  begin
    if i > 0 then
      Lines.Add('');
    Lines.Add(FRules[i].MatchPattern);
    Lines.Add(FRules[i].ReplaceTemplate);
  end;
end;

function ReverseFunctionPairStr(var S: string; _Rules: TReversalRuleList): Boolean;
var
  i: Integer;
  re: TRegExpr;
  Rule: TReversalRule;
begin
  Result := False;
  if (S = '') or (_Rules = nil) or (_Rules.Count = 0) then
    Exit;

  re := TRegExpr.Create;
  try
    re.ModifierI := True;
    for i := 0 to _Rules.Count - 1 do
    begin
      Rule := _Rules[i];
      try
        re.Expression := '^' + Rule.MatchPattern + '$';
        if re.Exec(S) then
        begin
          S := re.Substitute(Rule.ReplaceTemplate);
          Result := True;
          Exit;
        end;
      except
        on ERegExpr do
          Continue; // skip invalid patterns
      end;
    end;
  finally
    FreeAndNil(re);
  end;
end;

function ReverseAssignmentStr(var S: string; _IsCpp: Boolean): Boolean;
var
  i: Integer;
  AssignOp: string;
  AssignPos: Integer;
  SemPos: Integer;
  StringBefore: string;
  StringAfter: string;
  TrailingString: string;
  SpaceBefore: string;
  SpaceAfter: string;
  LeadingSpace: string;
begin
  if _IsCpp then
    AssignOp := '='
  else
    AssignOp := ':=';
  Result := False;
  if S = '' then
    Exit;
  AssignPos := Pos(AssignOp, S);
  SemPos := LastDelimiter(';', S);
  TrailingString := Copy(S, SemPos + 1);

  if (AssignPos > 1) and (SemPos > 3) and (Length(S) > AssignPos + 1) then
  begin
    if StrContains('//', S) then
      if Pos('//', S) < AssignPos then
        Exit;
    if StrContains('(*', S) then
      if Pos('(*', S) < AssignPos then
        Exit;
    if _IsCpp then
    begin
      if StrContains('/*', S) then
        if Pos('/*', S) < AssignPos then
          Exit;
      if Pos(S, '==') = AssignPos then
        Exit;
    end
    else
    begin
      if StrContains('{', S) then
        if Pos('{', S) < AssignPos then
          Exit;
    end;
    i := 1;
    while IsCharWhitespace(S[i]) do
    begin
      LeadingSpace := LeadingSpace + S[i];
      Inc(i);
    end;
    StringBefore := Copy(S, i, AssignPos - i);
    i := AssignPos - 1;
    if StringBefore <> '' then
    begin
      while IsCharWhitespace(S[i]) do
      begin
        SpaceBefore := S[i] + SpaceBefore;
        SetLength(StringBefore, Length(StringBefore) - 1);
        Dec(i)
      end;
    end;
    i := AssignPos + Length(AssignOp);
    while IsCharWhitespace(S[i]) do
    begin
      SpaceAfter := SpaceAfter + S[i];
      Inc(i);
    end;
    StringAfter := Copy(S, i, SemPos - i);
    S := LeadingSpace + StringAfter + SpaceAfter + AssignOp +
      SpaceBefore + StringBefore + ';' + TrailingString;
    Result := True;
  end;
end;

function ReverseForLoopStr(var S: string): Boolean;
const
  cForString = 'for ';
  cAssignString = ':=';
  cToString = ' to ';
  cDownToString = ' downto ';
  cDoString = ' do';
var
  Down: Boolean;
  iCurr, iPrev: Integer;
  LeadingString: string;
  TrailingString: string;
  StringBefore: string;
  StringAfter: string;

  function CopyS(iFrom, iTo: Integer): string;
  begin
    Result := Copy(S, iFrom, iTo - iFrom);
  end;

  function PosInSFromPrev(const Pat: string): Integer;
  begin
    Result := CaseInsensitivePosFrom(Pat, S, iPrev);
  end;

begin
  Result := False;

  iPrev := 1;
  iCurr := PosInSFromPrev(cForString);
  if iCurr = 0 then
    Exit;

  iPrev := iCurr;
  iCurr := PosInSFromPrev(cAssignString);
  if iCurr = 0 then
    Exit;
  Inc(iCurr, Length(cAssignString));
  LeadingString := CopyS(1, iCurr);

  iPrev := iCurr;
  iCurr := PosInSFromPrev(cDownToString);
  Down := iCurr > 0;
  if not Down then
  begin
    iCurr := PosInSFromPrev(cToString);
    if iCurr = 0 then
      Exit;
  end;
  StringBefore := CopyS(iPrev, iCurr);
  if Down then
    Inc(iCurr, Length(cDownToString))
  else
    Inc(iCurr, Length(cToString));

  iPrev := iCurr;
  iCurr := PosInSFromPrev(cDoString);
  if iCurr = 0 then
    Exit;
  StringAfter := CopyS(iPrev, iCurr);
  TrailingString := CopyS(iCurr, Succ(Length(S)));

  if Down then
    S := cToString
  else
    S := cDownToString;
  S := TrimRight(LeadingString) + ' ' + Trim(StringAfter) + S +
    Trim(StringBefore) + ' ' + TrimLeft(TrailingString);
  Result := True;
end;

end.
