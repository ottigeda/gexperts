// generates formatted Pascal code from a token collection
// Original Author:     Egbert van Nes (http://www.dow.wau.nl/aew/People/Egbert_van_Nes.html)
// Contributors:        Thomas Mueller (http://www.dummzeuch.de)
unit GX_CodeFormatterFormatter;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils,
  Classes,
  GX_CollectionLikeLists,
  GX_CodeFormatterTypes,
  GX_CodeFormatterStack,
  GX_CodeFormatterTokens,
  GX_CodeFormatterSettings;

type
  TCodeFormatterFormatter = class
  private
    FSettings: TCodeFormatterSettings;
    FTokens: TOCollection;
    FTokenIdx: Integer;
    FPrevToken: TPascalToken;
    FCurrentToken: TPascalToken;
    FCurrentRType: TReservedType;
    FPrevLine: TLineFeed;
    FHasAligned: Boolean;
    FStack: TCodeFormatterStack;
    FLastPopResType: TReservedType;
    procedure UppercaseCompilerDirective(_Token: TPascalToken);
    function NoBeginTryIndent(_rtype: TReservedType): Boolean;
    procedure SetPrevLineIndent(_Additional: integer);
    procedure DecPrevLineIndent;
    {: replaces a TExpression with a TAlignExpression }
    function AlignExpression(Idx: Integer; aPos: Integer): TPascalToken;
    procedure CheckWrapping;
    function PrevTokenIsRType(_rtype: TReservedType): boolean;
    procedure CheckBlankLinesAroundProc;
    procedure PutCommentBefore(aComment: PAnsiChar);
    procedure FormatAsm(NTmp: integer);
    procedure AdjustSpacing(_CurrentToken, _PrevToken: TPascalToken; _TokenIdx: Integer);

    {: return token with index Idx or nil if out of bounds }
    function GetToken(Idx: Integer): TPascalToken; overload;
    {: get token with index Idx, returns False if index is out of bounds }
    function GetToken(Idx: Integer; out Token: TPascalToken): Boolean; overload;
    {: Check whether the token at index AIdx has the reserved type ARType
       @param AIdx is the index of the token to check
       @param ARType is the queried reserverd type
       @returns true, if the token has the queried type, false otherwise }
    function TokenAtIs(AIdx: integer; ARType: TReservedType): boolean;

    function GetNextNoComment(StartPos: Integer; out Offset: Integer): TPascalToken; overload;
    function GetNextNoComment(StartPos: Integer; out Token: TPascalToken; out Offset: Integer): Boolean; overload;
    function GetNextNoComment(StartPos: Integer; out Token: TPascalToken): Boolean; overload;

    function InsertBlankLines(atIndex, NLines: Integer): TLineFeed;
    function AssertLineFeedAfter(StartPos: Integer): TLineFeed;
    {: This function does the actual formatting }
    procedure doExecute(ATokens: TOCollection);

    property Settings: TCodeFormatterSettings read FSettings write FSettings;
  public
    constructor Create(ASettings: TCodeFormatterSettings);
    destructor Destroy; override;
    class procedure Execute(ATokens: TOCollection; ASettings: TCodeFormatterSettings);
  end;

implementation

constructor TCodeFormatterFormatter.Create(ASettings: TCodeFormatterSettings);
begin
  inherited Create;
  FSettings := ASettings;
  FHasAligned := False;
  FPrevLine := nil;
  FStack := TCodeFormatterStack.Create;
end;

destructor TCodeFormatterFormatter.Destroy;
begin
  FStack.Free;
  inherited;
end;

class procedure TCodeFormatterFormatter.Execute(ATokens: TOCollection; ASettings: TCodeFormatterSettings);
var
  Formatter: TCodeFormatterFormatter;
begin
  Formatter := TCodeFormatterFormatter.Create(ASettings);
  try
    Formatter.doExecute(ATokens);
  finally
    Formatter.Free;
  end;
end;

function TCodeFormatterFormatter.AlignExpression(Idx: Integer; aPos: Integer): TPascalToken;
var
  OldExpr: TExpression;
begin
  FHasAligned := True;
  OldExpr := TExpression(FTokens.Items[Idx]);
  Result := TAlignExpression.Create(OldExpr, aPos);
  FTokens.Items[Idx] := Result;
  OldExpr.Free;
end;

procedure TCodeFormatterFormatter.AdjustSpacing(_CurrentToken, _PrevToken: TPascalToken; _TokenIdx: Integer);
var
  Prev2: TPascalToken;
  rtype: TReservedType;
  wType: TWordType;
  Idx: Integer;
begin
  if _CurrentToken = nil then
    Exit;
  rtype := _CurrentToken.ReservedType;
  wType := _CurrentToken.WordType;

  { TODO -otwm : This doesn't really belong here, it has nothing to do with spacing }
  if not (rtype in NoReservedTypes) then
    _CurrentToken.ExpressionCase := Settings.ReservedCase
  else if rtype in StandardDirectives then
    _CurrentToken.ExpressionCase := Settings.StandDirectivesCase
  else begin
    _CurrentToken.ExpressionCase := rfUnchanged;
    if (wType = wtWord) then
      Settings.HandleCapitalization(_CurrentToken);
  end;

  case rtype of
    rtThen, rtOf, rtElse, rtDo, rtAsm:
      _CurrentToken.SetSpace([spBefore, spAfter], True);
    rtEnd, rtFuncDirective:
      _CurrentToken.SetSpace([spBefore], True);
    rtIf, rtUntil, rtWhile, rtCase, rtRecord:
      _CurrentToken.SetSpace([spAfter], True);
    rtOper, rtMathOper, rtPlus, rtMinus, rtLogOper, rtEquals:
      _CurrentToken.SetSpace(Settings.SpaceOperators, True);
    rtEqualOper:
      _CurrentToken.SetSpace(Settings.SpaceEqualOper, True);
    rtColon:
      _CurrentToken.SetSpace(Settings.SpaceColon, True);
    rtSemiColon:
      _CurrentToken.SetSpace(Settings.SpaceSemiColon, True);
    rtComma:
      _CurrentToken.SetSpace(Settings.SpaceComma, True);
    rtLeftBr: begin
        _CurrentToken.SetSpace(Settings.SpaceLeftBr, True);
        if _PrevToken.ReservedType = rtLeftBr then
          _CurrentToken.SetSpace([spBefore], False);
      end;
    rtLeftHook: begin
        _CurrentToken.SetSpace(Settings.SpaceLeftHook, True);
        if _PrevToken.ReservedType = rtLeftHook then
          _CurrentToken.SetSpace([spBefore], False);
      end;
    rtRightBr:
      _CurrentToken.SetSpace(Settings.SpaceRightBr, True);
    rtRightHook:
      _CurrentToken.SetSpace(Settings.SpaceRightHook, True);
  end;
  {append space after : , ;}
  if (wType = wtHexNumber) and Settings.UpperNumbers then
    _CurrentToken.SetCase(rfUpperCase);
  {delimiter between 2 words (necessary)}
  if (_PrevToken <> nil) then begin
    if (Settings.SpaceOperators <> []) and
      (wType in [wtString, wtFullComment, wtHalfComment, wtHalfStarComment]) and
      not (_PrevToken.ReservedType in [rtDotDot, rtLineFeed]) then
      _CurrentToken.SetSpace([spBefore], True);
    if (rtype in [rtMinus, rtPlus]) then begin
      Prev2 := _PrevToken;
      Idx := 0;
      while (Prev2 <> nil) and (Prev2.ReservedType in [rtComment, rtLineFeed]) do begin
        Inc(Idx);
        if Idx > _TokenIdx then
          Prev2 := nil
        else
          Prev2 := FTokens.Items[_TokenIdx - Idx];
      end;
      if (Prev2 <> nil) and (Prev2.ReservedType in [rtOper,
        rtMathOper, rtPlus, rtMinus, rtSemiColon, rtOf,
          rtMinus, rtLogOper, rtEquals, rtEqualOper, rtLeftBr,
          rtLeftHook, rtComma, rtDefault]) then
        _CurrentToken.SetSpace([spAfter], False); {sign operator}
    end;
    if (rtype = rtLeftHook) then begin
      if not (_PrevToken.ReservedType in [rtReserved, rtNothing, rtRightBr, rtRightHook]) then
        //    PascalWord.SetSpace([spBefore], False) {array}
        //  else
        _CurrentToken.SetSpace([spBefore], True);
    end;
    if _CurrentToken.Space(spBefore)
      and (_PrevToken.ReservedType in [rtLeftBr, rtLeftHook, rtLineFeed]) then
      _CurrentToken.SetSpace([spBefore], False);
    if (_PrevToken.WordType in [wtWord, wtNumber, wtHexNumber, wtString])
      and (wType in [wtWord, wtNumber, wtHexNumber]) then
      _CurrentToken.SetSpace([spBefore], True);
    if (_PrevToken.ReservedType = rtComment)
      and (wType in [wtWord, wtNumber, wtHexNumber]) then
      _CurrentToken.SetSpace([spBefore], True);
    if _CurrentToken.Space(spBefore) and _PrevToken.Space(spAfter) then
      _PrevToken.SetSpace([spAfter], False); {avoid double spaces}
  end;
end;

function TCodeFormatterFormatter.TokenAtIs(AIdx: integer; ARType: TReservedType): boolean;
var
  Token: TPascalToken;
begin
  Result := GetToken(AIdx, Token);
  if Result then
    Result := (Token.ReservedType = ARType);
end;

function TCodeFormatterFormatter.GetToken(Idx: Integer): TPascalToken;
begin
  GetToken(Idx, Result);
end;

function TCodeFormatterFormatter.GetToken(Idx: Integer; out Token: TPascalToken): Boolean;
begin
  Result := (Idx >= 0) and (Idx < FTokens.Count);
  if Result then
    Token := TPascalToken(FTokens[Idx])
  else
    Token := nil;
end;

function TCodeFormatterFormatter.GetNextNoComment(StartPos: Integer; out Offset: Integer): TPascalToken;
begin
  if not GetNextNoComment(StartPos, Result, Offset) then
    Result := nil;
end;

function TCodeFormatterFormatter.GetNextNoComment(StartPos: Integer; out Token: TPascalToken; out Offset: Integer): Boolean;
begin
  Offset := 0;
  repeat
    Inc(Offset);
    Result := GetToken(StartPos + Offset, Token);
  until not Result or (Token.ReservedType <> rtComment);
end;

function TCodeFormatterFormatter.GetNextNoComment(StartPos: Integer; out Token: TPascalToken): Boolean;
var
  Offset: Integer;
begin
  Result := GetNextNoComment(StartPos, Token, Offset);
end;

function TCodeFormatterFormatter.InsertBlankLines(atIndex, NLines: Integer): TLineFeed;
var
  LineIdx: Integer;
  NextToken: TPascalToken;
begin
  Result := FPrevLine;
  for LineIdx := 0 to NLines - 1 do begin
    Result := TLineFeed.Create(0, Settings.SpacePerIndent);
    Result.SetIndent(FStack.nIndent);
    NextToken := GetToken(atIndex);
    { TODO -otwm -ccheck : is the if statement necessary? }
    if NextToken.Space(spBefore) then
      NextToken.SetSpace([spBefore], False);
    FTokens.Insert(atIndex, Result);
    AdjustSpacing(NextToken, Result, atIndex);
  end;
  if atIndex <= FTokenIdx then
    Inc(FTokenIdx, NLines);
end;

function TCodeFormatterFormatter.AssertLineFeedAfter(StartPos: Integer): TLineFeed;
var
  next: TPascalToken;
  Offset: Integer;
begin
  if GetNextNoComment(StartPos, next, Offset) and (next.ReservedType <> rtLineFeed) then
    Result := InsertBlankLines(StartPos + Offset, 1)
  else
    Result := FPrevLine;
end;

procedure TCodeFormatterFormatter.DecPrevLineIndent;
begin
  if FPrevLine <> nil then
    FPrevLine.IncIndent(-1);
end;

procedure TCodeFormatterFormatter.SetPrevLineIndent(_Additional: integer);
begin
  if FPrevLine <> nil then
    FPrevLine.SetIndent(FStack.nIndent + _Additional + FStack.ProcLevel);
end;

function TCodeFormatterFormatter.NoBeginTryIndent(_rtype: TReservedType): Boolean;
begin
  Result := not ((Settings.IndentBegin and (_rtype = rtBegin)) or
    (Settings.IndentTry and (_rtype = rtTry))) and
    (FStack.GetTopType in [rtDo, rtThen, rtIfElse]);
end;

procedure TCodeFormatterFormatter.UppercaseCompilerDirective(_Token: TPascalToken);
var
  Idx: integer;
  s: string;
begin
  _Token.GetExpression(s);
  Idx := 2;
  while (Idx < Length(s)) and (s[Idx] <> ' ') and (s[Idx] <> Tab) do begin
    s[Idx] := UpCase(s[Idx]);
    Inc(Idx);
  end;
  _Token.SetExpression(s);
end;

function TCodeFormatterFormatter.PrevTokenIsRType(_rtype: TReservedType): boolean;
begin
  Result := Assigned(FPrevToken) and (FPrevToken.ReservedType = _rtype);
end;

{: checks and corrects the number of blank lines before a procedure / function declaration }

procedure TCodeFormatterFormatter.CheckBlankLinesAroundProc;
var
  k: Integer;
  Prev2: TPascalToken;
begin
  if (FPrevToken <> nil) then begin
    k := 1;
    if FPrevToken.ReservedType = rtClass then begin
      // class procedure / function
      k := 2;
      Prev2 := GetToken(FTokenIdx - 2);
    end else // just procedure / function
      Prev2 := FPrevToken;
    if (Prev2 <> nil) and (Prev2.ReservedType <> rtLineFeed) then begin
      // no line feed at all -> add two for an empty line
      FPrevLine := InsertBlankLines(FTokenIdx - k, 2);
      FPrevToken := FPrevLine;
    end else begin
      // we got one linefeed already, check if there is another one -> empty line
      Inc(k);
      if GetToken(FTokenIdx - k, Prev2) and (Prev2.ReservedType <> rtLineFeed) then begin
        // no, only one -> add one for an empty line
        FPrevLine := InsertBlankLines(FTokenIdx - k + 1, 1);
        FPrevToken := FPrevLine;
      end;
    end;
  end;
end;

procedure TCodeFormatterFormatter.PutCommentBefore(aComment: PAnsiChar);
var
  J: Integer;
  P: TPascalToken;
  s: string;
begin
  J := FTokenIdx - 2;
  P := GetToken(J);
  { TODO -otwm : Does this work correctly in Delphi 2009? }
  SetString(s, aComment, StrLen(aComment));
  if P.ReservedType = rtComment then
    P.SetExpression(s)
  else begin
    P := TExpression.Create(wtWord, s);
    P.SetReservedType(rtComment);
    FTokens.Insert(FTokenIdx, P);
    Inc(FTokenIdx);
    P := TLineFeed.Create(0, Settings.SpacePerIndent);
    TLineFeed(P).SetIndent(FStack.nIndent);
    FTokens.Insert(FTokenIdx, P);
    Inc(FTokenIdx);
  end;
end;

// When we enter this method FCurrentToken is 'asm' and FCurrentRType os rtAsm

procedure TCodeFormatterFormatter.FormatAsm(NTmp: integer);
begin
  // remove var / type stuff
  while FStack.GetTopType in [rtVar, rtType] do
    FStack.Pop;
  // no additional indentation for
  // procedure xxx;
  // asm
  if FStack.GetTopType = rtProcedure then begin
    FStack.Pop;
    DecPrevLineIndent;
  end;
  FStack.Push(FCurrentRType, 0);

  // twm: now we handle all asm statements until we hit an 'end'
  // rather ugly
  FCurrentToken := GetToken(FTokenIdx);
  while (FTokenIdx < FTokens.Count - 1) and (FCurrentToken.ReservedType <> rtEnd) do begin
    if FCurrentToken.ReservedType = rtLineFeed then begin
      FPrevLine := TLineFeed(FCurrentToken);
      with FPrevLine do
        FNoOfSpaces := FOldNoOfSpaces;
    end;
    AdjustSpacing(FCurrentToken, FPrevToken, FTokenIdx);
    Inc(FTokenIdx);
    FPrevToken := FCurrentToken;
    FCurrentToken := GetToken(FTokenIdx);
  end;
  if FTokenIdx < FTokens.Count then
    SetPrevLineIndent(NTmp);
  Dec(FTokenIdx);
end;

procedure TCodeFormatterFormatter.doExecute(ATokens: TOCollection);
var
  OldWrapIndent: Boolean; // stores WrapIndent from before an opening bracket until the closing one
  PrevPrevLine: TLineFeed;
  StackStack: TCodeFormatterStackStack;
  WrapIndent: Boolean;
  InInterfacePart: Boolean; // True between 'interface' and 'implementation'
  NTmp: Integer;
  PrevOldNspaces: Integer;

  procedure CheckIndent;

    procedure CheckSlashComment;
    var
      Token: TPascalToken;
      PrevPasWord: TPascalToken;
      Expression: string;
      PrevExpression: string;
      i: Integer;
    begin
      if GetToken(FTokenIdx - 1, FPrevToken) and (FPrevToken.ReservedType = rtComment)
        and FPrevToken.GetExpression(PrevExpression)
        and (PrevExpression[1] = '/') then {fix for situation with a // comment
        on prev line: begin becomes part of the comment}
        if not FPrevToken.ChangeComment('{') then begin
          i := 0;
          Token := nil;
          repeat
            PrevPasWord := Token;
            Token := GetToken(FTokenIdx + i);
            Inc(i);
          until (Token = nil) or (Token.ReservedType = rtLineFeed);
          Dec(i);
          if (PrevPasWord.ReservedType = rtComment)
            and PrevPasWord.GetExpression(Expression)
            and (Expression[1] = '/') then begin
            FPrevToken.SetExpression('{' + Copy(PrevExpression, 2, 999999) + '}');
            Exit;
          end else
            FTokens.Delete(FTokenIdx - 1);
          FTokens.Insert(FTokenIdx + i, FPrevToken);
          FPrevToken := GetToken(FTokenIdx - 1);
          AdjustSpacing(FPrevToken, GetToken(FTokenIdx - 2), FTokenIdx - 1);
          FCurrentToken := GetToken(FTokenIdx);
        end;
      FPrevLine := PrevPrevLine;
    end;

    procedure CheckShortLine;
    var
      Token: TPascalToken;

      function TokenRType: TReservedType;
      begin
        if Token = nil then
          Result := rtNothing
        else
          Result := Token.ReservedType;
      end;

    var
      Offset: integer;
    begin { CheckShortLine }
      Offset := 1;
      Token := GetToken(FTokenIdx + Offset);
      if TokenRType = rtLineFeed then begin
        while not ((TokenRType in [rtSemiColon, rtBegin, rtElse, rtDo, rtWhile, rtOn, rtThen, rtCase])
          or ((Offset > 1) and (Token.ReservedType = rtLineFeed))) do begin
          Inc(Offset);
          Token := GetToken(FTokenIdx + Offset);
        end;
        if TokenRType = rtSemiColon then begin
          FTokens.AtFree(FTokenIdx + 1);
        end;
      end;
    end;

    procedure ComplexIfElse;
    begin
      while not FStack.IsEmpty and (FLastPopResType <> rtThen) do begin
        FLastPopResType := FStack.Pop;
        if FLastPopResType = rtIfElse then
          ComplexIfElse;
      end;
      SetPrevLineIndent(NTmp);
    end;

  var
    RemoveMe: Integer; // twm: k is used in several sub procedures but it should be possible to declare local variables in all of them
    next: TPascalToken;
    TempWordIdx: Integer;
    Prev1: TPascalToken;
    functdeclare, NoBlankLine: Boolean;
    FeedRound: TFeedBegin;
    WType: TWordType;

  begin { procedure CheckIndent; }
    if FCurrentToken = nil then
      exit;

    FCurrentRType := FCurrentToken.ReservedType;
    WType := FCurrentToken.WordType;
    { This handles the case where a reserved word was used as the name of
      a class member. Is that even allowed? }
    if (FCurrentRType in [rtWhile, rtEnd, rtRepeat, rtBegin, rtUses, rtTry,
      rtProgram, rtType, rtvar, rtIf, rtThen, rtElse] + standardDirectives)
      and PrevTokenIsRType(rtDot) then begin
      FCurrentToken.SetReservedType(rtNothing);
      FCurrentRType := rtNothing;
    end;

    {SetSpacing;}
    case FCurrentRType of
      rtIf: begin
          if Settings.FeedAfterThen and not Settings.FeedElseIf
            and (FStack.GetTopType = rtIfElse) and (FPrevToken = FPrevLine) then begin
            FTokens.AtFree(FTokenIdx - 1);
            Dec(FTokenIdx);
            CheckSlashComment;
          end else begin
            if Settings.FeedElseIf and (FPrevToken <> FPrevLine) then begin
              FPrevLine := AssertLineFeedAfter(FTokenIdx - 1);
              FPrevToken := FPrevLine;
            end;
          end;
          if PrevTokenIsRType(rtElse)
            or (Settings.NoIndentElseIf and (FStack.GetTopType = rtIfElse)) then begin
            FStack.Pop;
            if FStack.GetTopType = rtThen then
              FStack.Pop;
            WrapIndent := True;
            FStack.Push(rtIfElse, 0);
          end else
            FStack.Push(rtIf, 0);
        end;
      rtThen:
        if FStack.GetTopType in [rtIf, rtIfElse] then begin
          WrapIndent := False;
          FLastPopResType := FStack.Pop;
          if Settings.NoFeedBeforeThen and (FPrevToken = FPrevLine)
            and (GetToken(FTokenIdx - 1).ReservedType <> rtComment) then begin
            FTokens.AtFree(FTokenIdx - 1);
            Dec(FTokenIdx);
            CheckSlashComment;
          end;
          if Settings.FeedAfterThen then begin
            if AssertLineFeedAfter(FTokenIdx) <> FPrevLine then begin
              if (FLastPopResType = rtIf) and Settings.ExceptSingle then
                CheckShortLine;
            end;
          end;
          FStack.Push(rtThen, 1);
        end;
      rtColon: begin
          case FStack.GetTopType of
            rtOf: begin
                FStack.Push(FCurrentRType, 1);
                if Settings.FeedAfterThen then begin
                  if (GetNextNoComment(FTokenIdx, RemoveMe).ReservedType = rtBegin) and
                    (AssertLineFeedAfter(FTokenIdx) <> FPrevLine) then
                    CheckShortLine;
                end;
                WrapIndent := False;
              end;
            rtClassDecl: begin
                FStack.Pop;
                FStack.Push(rtClass, 1);
              end;
            rtVar:
              if Settings.AlignVar then
                FCurrentToken := AlignExpression(FTokenIdx, Settings.AlignVarPos);
            rtProcedure, rtProcDeclare:
              ; // do nothing
          else
            //label????
            WrapIndent := False;
          end;
        end;
      rtElse: begin
          FLastPopResType := rtNothing;
          while not FStack.IsEmpty and not (FStack.GetTopType in [rtThen, rtOf, rtTry]) do
            FLastPopResType := FStack.Pop;
          if FLastPopResType = rtIfElse then
            ComplexIfElse;
          if (Settings.FeedRoundBegin = Hanging)
            and (FPrevToken <> nil)
            and TokenAtIs(FTokenIdx - 1, rtLineFeed)
            and TokenAtIs(FTokenIdx - 2, rtEnd) then begin
            FTokens.AtFree(FTokenIdx - 1);
            Dec(FTokenIdx);
            FPrevLine := nil;
            FPrevToken := FPrevLine;
          end;
          if Settings.FeedAfterThen then begin
            if (FPrevToken <> nil)
              and ((Settings.FeedRoundBegin <> Hanging) or not TokenAtIs(FTokenIdx - 1, rtEnd))
              and not TokenAtIs(FTokenIdx - 1, rtLineFeed) then begin
              FPrevLine := AssertLineFeedAfter(FTokenIdx - 1);
              FPrevToken := FPrevLine;
            end;
            if GetNextNoComment(FTokenIdx, Next)
              and (Next.ReservedType <> rtIf) then
              AssertLineFeedAfter(FTokenIdx);
          end;
          FStack.GetTopIndent;
          if FPrevToken = FPrevLine then
            SetPrevLineIndent(NTmp);
          if Settings.IndentTryElse and (FStack.GetTopType = rtTry) then begin
            FStack.NIndent := FStack.NIndent + 1;
            SetPrevLineIndent(NTmp);
          end else if Settings.IndentCaseElse and (FStack.GetTopType = rtOf) then begin
            FStack.NIndent := FStack.NIndent + 1;
            SetPrevLineIndent(NTmp);
          end;
          if FStack.GetTopType = rtThen then
            FStack.Push(rtIfElse, 1)
          else
            FStack.Push(rtElse, 1);
          WrapIndent := False;
        end;
      rtRepeat, rtRecord: begin
          FStack.Push(FCurrentRType, 1);
          WrapIndent := False;
        end;
      rtClass: begin
          if not (GetNextNoComment(FTokenIdx, next)
            and (next.ReservedType in [rtProcedure, rtProcDeclare, rtOf])) then begin
            { not a "class function" or "class of" declaration }
            WrapIndent := False;
            FStack.Push(rtClassDecl, 1);
          end else
            { first assume that it is a class declaration
              the first procedure replaces it with rtClass }
            FCurrentToken.SetSpace([spAfter], True);
        end;
      rtUntil: begin
          repeat
            FLastPopResType := FStack.Pop;
          until (FLastPopResType = rtRepeat) or FStack.IsEmpty;
          SetPrevLineIndent(NTmp);
        end;
      rtLeftBr:
        if (FStack.GetTopType = rtLeftBr) then
          FStack.Push(FCurrentRType, 0)
        else begin
          OldWrapIndent := WrapIndent;
          if (FStack.ProcLevel <= 0) or (FStack.GetTopType <> rtProcedure) then
            {niet erg netjes}
            FStack.Push(FCurrentRType, 1)
          else begin
            RemoveMe := 1;
            while (FTokenIdx > RemoveMe) and (GetToken(FTokenIdx - RemoveMe, next)
              and (next.ReservedType in [rtDot, rtNothing])) do begin
              Inc(RemoveMe);
            end;
            if (next <> nil) and (next.ReservedType = rtProcedure) then
              FStack.Push(FCurrentRType, 0)
            else
              FStack.Push(FCurrentRType, 1);
          end;
          WrapIndent := False;
        end;
      rtLeftHook, rtWhile, rtOn: // left hook = '['
        FStack.Push(FCurrentRType, 0);
      rtRightBr: begin
          repeat
            FLastPopResType := FStack.Pop;
          until (FLastPopResType = rtLeftBr) or FStack.IsEmpty;
          if not (FStack.GetTopType = rtLeftBr) then
            WrapIndent := OldWrapIndent;
        end;
      rtRightHook: begin // right hook = ']'
          repeat
            FLastPopResType := FStack.Pop;
          until (FLastPopResType = rtLeftHook) or FStack.IsEmpty;
          if FStack.GetTopType = rtClassDecl {Interface} then
            WrapIndent := False;
        end;
      rtExcept: begin
          while not FStack.IsEmpty and (FStack.GetTopType <> rtTry) do
            FStack.Pop;
          FStack.GettopIndent;
          SetPrevLineIndent(NTmp);
          FStack.NIndent := FStack.NIndent + 1;
          WrapIndent := False;
        end;
      rtVisibility:
        if not (FStack.GetTopType in [rtClass, rtClassDecl, rtRecord]) then
          FCurrentToken.SetReservedType(rtNothing)
        else if PrevTokenIsRType(rtLineFeed) then begin
          DecPrevLineIndent;
          WrapIndent := False;
        end;
      rtOf: begin
          case FStack.GetTopType of
            rtCase: begin
                FStack.Push(FCurrentRType, 1);
                if Settings.FeedAfterThen then
                  AssertLineFeedAfter(FTokenIdx);
                WrapIndent := False;
              end;
            rtRecord: WrapIndent := False;
          end;
        end;
      rtLineFeed: begin
          if FStack.IsEmpty then
            WrapIndent := False;
          if Settings.RemoveDoubleBlank and (FTokenIdx >= 2) and (FPrevToken <> nil)
            and (FPrevToken = FPrevLine) and
            (FTokens.Items[FTokenIdx - 2] = PrevPrevLine) then begin
            FTokens.AtFree(FTokenIdx - 2);
            Dec(FTokenIdx);
          end;
          if GetNextNoComment(FTokenIdx, next) then begin
            if next.ReservedType in [rtElse, rtIfElse, rtBegin, rtEnd, rtUntil, rtExcept] then
              WrapIndent := False;
            {TLineFeed(PascalWord).Wrapped:=WrapIndent;}
            if WrapIndent then
              NTmp := 1
            else
              NTmp := 0;
            WrapIndent := True;
            if (next.ReservedType in [rtLineFeed])
              or (FStack.GetTopType in [rtUses, rtLeftBr]) then
              WrapIndent := False;
          end;
          PrevPrevLine := FPrevLine;
          FPrevLine := TLineFeed(FCurrentToken);
          SetPrevLineIndent(NTmp);
        end;
      rtAsm: begin
          FormatAsm(NTmp);
          Exit;
        end;
      rtComma:
        if Settings.FeedEachUnit and (FStack.GetTopType = rtUses) then begin
          next := GetNextNoComment(FTokenIdx, RemoveMe);
          if next.ReservedType <> rtLineFeed then begin
            AssertLineFeedAfter(FTokenIdx);
          end;
        end;
      rtProgram, rtUses, rtInitialization:
        if FStack.GetTopType <> rtLeftBr then begin
          next := GetNextNoComment(FTokenIdx, RemoveMe);
          if (FCurrentRType = rtUses) and (FStack.GetTopType in [rtProcedure, rtProcDeclare, rtClass]) then
            FCurrentToken.SetReservedType(rtNothing)
          else begin
            DecPrevLineIndent;
            FStack.Clear;
            FStack.Push(FCurrentRType, 1);
            WrapIndent := False;
          end;
          {nIndent := 1;}
        end;
      rtAbsolute:
        if not (FStack.GetTopType in [rtVar, rtType]) then
          FCurrentToken.SetReservedType(rtNothing)
        else begin
          next := GetNextNoComment(FTokenIdx, RemoveMe);
          if next.ReservedType = rtColon then begin
            DecPrevLineIndent;
            FCurrentToken.SetReservedType(rtNothing);
          end;
        end;
      rtFuncDirective, rtDefault: begin
          next := GetNextNoComment(FTokenIdx, RemoveMe);
          if (next.ReservedType = rtColon)
            or not (FStack.GetTopType in [rtProcedure, rtProcDeclare, rtClass])
            or (FPrevToken.ReservedType in [rtProcedure, rtProcDeclare, rtDot]) then
            FCurrentToken.SetReservedType(rtNothing);
        end;
      rtForward: begin
          if FStack.GetTopType in [rtProcedure, rtProcDeclare] then
            FStack.Pop
          else
            FCurrentToken.SetReservedType(rtNothing);
        end;
      rtProcedure: begin
          if FStack.GetTopType in [rtClassDecl, rtRecord] then begin
            FStack.Pop;
            FStack.Push(rtClass, 1);
          end;
          Prev1 := FPrevToken;
          TempWordIdx := FTokenIdx;
          if Prev1 <> nil then begin
            while (TempWordIdx > 0) and (Prev1.ReservedType in [rtComment, rtLineFeed]) do begin
              Dec(TempWordIdx);
              Prev1 := FTokens.Items[TempWordIdx];
            end;
            functdeclare := (Prev1 <> nil) and (Prev1.ReservedType in [rtEquals, rtColon]);
          end else
            functdeclare := False;
          NoBlankLine := False;
          if not functdeclare then begin
            RemoveMe := 0;
            repeat
              Inc(RemoveMe);
              if GetToken(FTokenIdx + RemoveMe, next) and (next.ReservedType = rtLeftBr) then
                repeat
                  Inc(RemoveMe);
                until not GetToken(FTokenIdx + RemoveMe, next) or (next.ReservedType = rtRightBr);
            until (next = nil) or (next.ReservedType = rtSemiColon);
            if next <> nil then begin
              repeat
                Inc(RemoveMe);
              until not GetToken(FTokenIdx + RemoveMe, next) or not (next.ReservedType in [rtLineFeed, rtComment]);
              if (next <> nil) and (next.ReservedType = rtForward) then
                NoBlankLine := True;
            end;
          end;
          if not (functdeclare or InInterfacePart or (FStack.GetTopType = rtClass)) then begin
            if not FStack.HasType(rtProcedure) then begin
              if (FStack.nIndent > 0) then begin
                FStack.nIndent := 0;
                SetPrevLineIndent(NTmp);
              end;
              FStack.ProcLevel := 0;
              if Settings.BlankProc and not NoBlankLine then
                CheckBlankLinesAroundProc;
              if Settings.CommentFunction then
                PutCommentBefore('{ procedure }');
            end else begin
              if Settings.BlankSubProc and not NoBlankLine then
                CheckBlankLinesAroundProc;
              FStack.ProcLevel := FStack.ProcLevel + 1;
              if FStack.nIndent = 0 then begin
                SetPrevLineIndent(NTmp);
                FStack.NIndent := FStack.NIndent + 1;
              end;
            end;
            FStack.Push(rtProcedure, 0);
          end else begin
            if (not functdeclare) and (not (FStack.GetTopType = rtClass)) then begin
              FStack.nIndent := 0;
              SetPrevLineIndent(NTmp);
            end;
            FStack.Push(rtProcDeclare, 0);
          end;
        end;
      rtInterface: begin
          if PrevTokenIsRType(rtEquals) then begin
            {declaration of a OLE object: IClass = interface [' dfgsgdf']}
            FStack.Push(rtClassDecl, 1);
          end else begin
            InInterfacePart := True;
            DecPrevLineIndent;
          end;
          WrapIndent := False;
        end;
      rtImplementation: begin
          FStack.Clear;
          InInterfacePart := False;
          WrapIndent := False;
          {DecPrevIndent;}
          {nIndent := 0;}
          SetPrevLineIndent(NTmp);
        end;
      rtBegin, rtTry: begin
          if FStack.GetTopType in [rtVar, rtType] then
            FStack.Pop;
          if FStack.GetTopType in [rtProcedure, rtProgram] then
            FStack.Pop;
          if FStack.IsEmpty then
            FStack.nIndent := 0;
          if NoBeginTryIndent(FCurrentRType) then
            FStack.NIndent := FStack.NIndent - 1;
          case FCurrentRType of
            rtBegin:
              FeedRound := Settings.FeedRoundBegin;
            rtTry:
              FeedRound := Settings.FeedRoundTry;
          else
            FeedRound := Unchanged;
          end;
          case FeedRound of
            Hanging: begin
                if (FStack.GetTopType in [rtDo, rtThen, rtIfElse, rtElse, rtColon])
                  and (FPrevToken <> nil) and (GetToken(FTokenIdx - 1) = FPrevLine) then begin
                  FTokens.AtFree(FTokenIdx - 1);
                  Dec(FTokenIdx);
                  CheckSlashComment;
                end;
                AssertLineFeedAfter(FTokenIdx);
              end;
            NewLine: begin
                if (FPrevToken <> nil) and (GetToken(FTokenIdx - 1).ReservedType <> rtLineFeed) then begin
                  FPrevLine := AssertLineFeedAfter(FTokenIdx - 1);
                  FPrevToken := FPrevLine;
                end;
                AssertLineFeedAfter(FTokenIdx);
              end;
          end;
          FStack.Push(FCurrentRType, 1);
          if FPrevToken = FPrevLine then begin
            SetPrevLineIndent(NTmp);
            DecPrevLineIndent;
          end;
          WrapIndent := False;
        end;
      rtEquals:
        if Settings.AlignVar and (FStack.GetTopType = rtVar) then
          FCurrentToken := AlignExpression(FTokenIdx, Settings.AlignVarPos);
      rtVar, rtType:
        if not (FStack.GetTopType in [rtLeftBr, rtLeftHook]) then begin
          WrapIndent := False;
          if FStack.nIndent < 1 then
            FStack.nIndent := 1;
          {in classes.pas I found
          t =  type AnsiString}
          if (FStack.GetTopType in [rtVar, rtType]) then
            FStack.Pop;
          FStack.Push(FCurrentRType, 0);
          if not PrevTokenIsRType(rtEquals) then begin
            DecPrevLineIndent;
            if Settings.FeedAfterVar then
              AssertLineFeedAfter(FTokenIdx);
          end;
        end;
      rtCase:
        if not (FStack.GetTopType in [rtRecord, rtLeftBr]) then
          FStack.Push(FCurrentRType, 0)
        else begin
          WrapIndent := False;
          FStack.Push(rtRecCase, 1);
        end;
      rtDo:
        if FStack.GetTopType in [rtWhile, rtOn] then begin
          FLastPopResType := FStack.GetTopType;
          FStack.Push(FCurrentRType, 1);
          WrapIndent := False;
          if Settings.NoFeedBeforeThen and (FPrevToken = FPrevLine) then begin
            FTokens.AtFree(FTokenIdx - 1);
            Dec(FTokenIdx);
            CheckSlashComment;
          end;
          if Settings.FeedAfterThen then begin
            if AssertLineFeedAfter(FTokenIdx) <> FPrevLine then begin
              if (FLastPopResType = rtOn) and Settings.ExceptSingle then
                CheckShortLine;
            end;
          end;
        end;
      rtEnd: begin
          WrapIndent := False;
          repeat
            FLastPopResType := FStack.Pop;
          until FStack.IsEmpty or (FLastPopResType in [rtClass, rtClassDecl, rtRecord, rtTry, rtCase, rtBegin, rtAsm]);
          if FStack.IsEmpty then
            FStack.nIndent := 0;
          if Settings.FeedBeforeEnd and (FPrevToken <> nil)
            and (GetToken(FTokenIdx - 1).ReservedType <> rtLineFeed) then begin
            FPrevLine := AssertLineFeedAfter(FTokenIdx - 1);
            FPrevToken := FPrevLine;
          end;
          if (FPrevToken = FPrevLine) then
            SetPrevLineIndent(NTmp);
          if NoBeginTryIndent(FCurrentRType) then
            FStack.NIndent := FStack.NIndent + 1;
        end;
      rtComment: begin
          if Settings.IndentComments and (FStack.GetTopType <> rtLeftHook) then
            WrapIndent := False;
          { TODO -otwm -ccheck : Why check for nIndent > 0? }
          if FStack.IsEmpty and (FStack.nIndent > 0) then begin
            FStack.nIndent := 0;
            SetPrevLineIndent(NTmp);
          end;
          AdjustSpacing(GetToken(FTokenIdx + 1), FCurrentToken, FTokenIdx + 1);
          if (FPrevLine <> nil) and (FPrevLine = FPrevToken) then begin
            if not Settings.IndentComments
              or (FCurrentToken.WordType in [wtFullOutComment, wtHalfOutComment]) then
              FPrevLine.FNoOfSpaces := FPrevLine.FOldNoOfSpaces
            else begin
              if PrevOldNspaces >= 0 then
                FPrevLine.FNoOfSpaces := FPrevLine.FNoOfSpaces +
                  (FPrevLine.FOldNoOfSpaces - PrevOldNspaces)
              else
                PrevOldNspaces := FPrevLine.FOldNoOfSpaces;
            end;
          end else if Settings.AlignComments and (FCurrentToken.WordType = wtFullComment) then begin
            if GetToken(FTokenIdx + 1, next) and (next.ReservedType = rtLineFeed) then
              FCurrentToken := AlignExpression(FTokenIdx, Settings.AlignCommentPos);
          end;
        end;
      rtSemiColon:
        if not (FStack.GetTopType in [rtLeftBr, rtLeftHook]) then begin
          while not FStack.IsEmpty and (FStack.GetTopType in [rtDo, rtWhile,
            rtProcDeclare, rtThen, rtProgram, rtUses, rtColon, rtClassDecl])
            or (FStack.GetTopType = rtIfElse) do
            FStack.Pop;
          WrapIndent := False;
          RemoveMe := 0;
          repeat
            Inc(RemoveMe);
          until not GetToken(FTokenIdx + RemoveMe, next) or (not (next.ReservedType in [{rtComment,}rtLineFeed]));
          if (next <> nil) then begin
            if (next.ReservedType = rtAbsolute)
              or ((FStack.GetTopType in [rtProcedure, rtProcDeclare, rtClass])
              and (next.ReservedType in [rtFuncDirective, rtForward])
              and (FStack.ProcLevel = 0)) then
              WrapIndent := True
            else if Settings.FeedAfterSemiColon
              and not (next.ReservedType in [rtForward, rtFuncDirective, rtDefault]) then
              AssertLineFeedAfter(FTokenIdx);
          end;
        end;
      rtCompIf: begin
          // push current stack to preserve indenting from before the ifdef
          StackStack.Push(FStack.Clone);
        end;
      rtCompElse: begin
          if not StackStack.IsEmpty then begin
            // Free current stack and take a copy of the one from before the corresponding ifdef.
            FStack.Free;
            FStack := StackStack.Top.Clone;
          end;
        end;
      rtCompEndif: begin
          // pop and free the saved stack
          if not StackStack.IsEmpty then
            StackStack.Pop.Free;
        end;
    end; // case FCurrentRType

    AdjustSpacing(FCurrentToken, FPrevToken, FTokenIdx);
    if not (FCurrentRType in [rtLineFeed, rtComment]) then
      PrevOldNspaces := -1;
    if WType = wtCompDirective then begin
      WrapIndent := False;
      if not Settings.IndentCompDirectives and PrevTokenIsRType(rtLineFeed) then begin
        NTmp := -FStack.nIndent;
        SetPrevLineIndent(NTmp);
      end;
      if Settings.UpperCompDirectives then begin
        UppercaseCompilerDirective(FCurrentToken);
      end;
    end;
    if not (FCurrentToken.ReservedType = rtComment) then
      FPrevToken := FCurrentToken;
  end;

begin {procedure TPascalParser.doExecute;}
  FTokens := ATokens;
  if Settings.ChangeIndent then begin
    FPrevLine := nil;
    PrevPrevLine := nil;
    FPrevToken := nil;
    WrapIndent := True;
    InInterfacePart := False;
    NTmp := 0;
    PrevOldNspaces := -1;
    try
      // the StackStack is used to preserve indenting over IFDEF/ELSE/ENDIF statments
      StackStack := TCodeFormatterStackStack.Create;
      FTokenIdx := 0;
      while GetToken(FTokenIdx, FCurrentToken) do begin
        CheckIndent;
        Inc(FTokenIdx);
      end;
    finally
      FreeAndNil(StackStack);
    end;
  end;

  // remove empty lines from the end
  FTokenIdx := FTokens.Count - 1;
  while (FTokenIdx > 0) and TokenAtIs(FTokenIdx, rtLineFeed) do begin
    FTokens.AtFree(FTokenIdx);
    Dec(FTokenIdx);
  end;

  if Settings.WrapLines or FHasAligned then
    CheckWrapping;
end;

procedure TCodeFormatterFormatter.CheckWrapping;
var
  HasInserted: Boolean;

  procedure InsertLinefeed(ATokenIdx: Integer);
  var
    PrevPrevLine: TLineFeed;
  begin
    PrevPrevLine := FPrevLine;
    FPrevLine := TLineFeed.Create(PrevPrevLine.FOldNoOfSpaces, Settings.SpacePerIndent);
    FPrevLine.FNoOfSpaces := PrevPrevLine.FNoOfSpaces;
    FPrevLine.FWrapped := True;
    GetToken(ATokenIdx).SetSpace([spBefore], False);
    FTokens.Insert(ATokenIdx, FPrevLine);
    HasInserted := True;
  end;

var
  TokenIdx, J, k: Integer;
  K2, LineLen: Integer;
  Token: TPascalToken;
  Expression: TAlignExpression;

begin
  LineLen := 0;
  FPrevLine := nil;
  J := 0;
  TokenIdx := 0;
  while TokenIdx < FTokens.Count do begin
    Token := GetToken(TokenIdx);
    // GetLength as a side effect, adjusts the alignment
    Token.GetLength(LineLen);
    if Settings.WrapLines and (Token is TAlignExpression)
      and (LineLen > Settings.WrapPosition) then begin
      Expression := Token as TAlignExpression;
      k := Expression.FNoOfSpaces - LineLen - Settings.WrapPosition;
      if k < 1 then
        Expression.FNoOfSpaces := 1
      else
        Expression.FNoOfSpaces := k;
      LineLen := Settings.WrapPosition;
    end;
    if Token.ReservedType = rtLineFeed then begin
      FPrevLine := TLineFeed(Token);
      if (LineLen > Settings.WrapPosition) then
        LineLen := 0;
      J := TokenIdx;
    end;
    if Settings.WrapLines and (LineLen > Settings.WrapPosition) and (TokenIdx > J + 3) then begin
      k := TokenIdx - 1;
      K2 := 0;
      HasInserted := False;
      while (k >= J) and not HasInserted do begin
        if (GetToken(k).ReservedType in [rtThen, rtDo])
          or (GetToken(k).ReservedType = rtElse)
          and (GetToken(k + 1).ReservedType <> rtIf) then begin
          InsertLinefeed(k + 1);
          TokenIdx := J;
        end;
        if (K2 = 0) and (GetToken(k).Space(spAfter) or
          GetToken(k + 1).Space(spBefore)) then
          K2 := k + 1;
        Dec(k);
      end;
      if not HasInserted and (K2 <> 0) and (K2 > J) then begin
        InsertLinefeed(K2);
        TokenIdx := J;
      end;
      LineLen := 0;
    end;
    Inc(TokenIdx);
  end;
end;

end.

