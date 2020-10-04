// generates formatted Pascal code from a token collection
// Original Author:     Egbert van Nes (http://www.dow.wau.nl/aew/People/Egbert_van_Nes.html)
// Contributors:        Thomas Mueller (http://www.dummzeuch.de)
//                      Jens Borrisholt (Jens@borrisholt.dk) - Cleaning up the code, and making it aware of several language features

unit GX_CodeFormatterFormatter;

{$I GX_CondDefine.inc}

// This unit uses Assert(False, 'some text') for trace logging (for the line numbers)
// Therefore we must turn off Assertions by default.
{$C-}
{$IFDEF ASSERT_TRACING}
// Assertions should only be turned on in the unit tests
{$C+}
{$ENDIF}

interface

uses
  SysUtils,
  Classes,
  GX_GenericUtils,
  GX_CodeFormatterTypes,
  GX_CodeFormatterStack,
  GX_CodeFormatterTokens,
  GX_CodeFormatterTokenList,
  GX_CodeFormatterSettings,
  GX_CodeFormatterUnicode;

type
  TCodeFormatterFormatter = class
  private
    FSettings: TCodeFormatterSettings;
    FTokens: TPascalTokenList;
    FTokenIdx: Integer;
    FPrevToken: TPascalToken;
    FCurrentToken: TPascalToken;
    FCurrentRType: TReservedType;
    FPrevLine: TLineFeed;
    FPrevPrevLine: TLineFeed;
    FHasAligned: Boolean;
    // the StackStack is used to preserve indenting over IFDEF/ELSE/ENDIF statements
    FStackStack: TCodeFormatterStack;
    FStack: TCodeFormatterSegment;
    FLastPopResType: TReservedType;
    // True between 'interface' and 'implementation'
    FIsInInterfacePart: Boolean;
    FWrapIndent: Boolean;
    // stores WrapIndent from before an opening bracket until the closing one
    FOldWrapIndent: Boolean;
    procedure UppercaseCompilerDirective(_Token: TPascalToken); {$IFDEF SupportsInline} inline; {$ENDIF}
    function NoBeginTryIndent(_rType: TReservedType): Boolean; {$IFDEF SupportsInline} inline; {$ENDIF}
    procedure IndentProcedureComment;
    procedure SetPrevLineIndent(_Additional: Integer); {$IFDEF SupportsInline} inline; {$ENDIF}
    procedure DecPrevLineIndent; {$IFDEF SupportsInline} inline; {$ENDIF}

    {: replaces a TExpression with a TAlignExpression }
    function AlignExpression(_Idx: Integer; _Pos: Integer): TPascalToken;
    procedure CheckWrapping;
    function IsRType(_Token: TPascalToken; _rType: TReservedType): Boolean; overload; {$IFDEF SupportsInline} inline; {$ENDIF}
    function IsRType(_Token: TPascalToken; _rTypeSet: TReservedTypeSet): Boolean; overload; {$IFDEF SupportsInline} inline; {$ENDIF}
    function IsWType(_Token: TPascalToken; _wType: TWordType): Boolean; {$IFDEF SupportsInline} inline; {$ENDIF}
    ///<summary>
    /// Checks and corrects the number of blank lines before a procedure / function declaration </summary>
    procedure CheckBlankLinesAroundProc;
    //procedure PutCommentBefore(const _Comment: TGXUnicodeString);
    procedure FormatAsm(_NTmp: Integer);
    procedure AdjustSpacing(_CurrentToken, _PrevToken: TPascalToken; _TokenIdx: Integer);

    ///<summary>
    /// @returns the token at index Idx or nil if out of bounds </summary>
    function GetToken(_Idx: Integer): TPascalToken; {$IFDEF SupportsInline} inline; {$ENDIF}
    ///<summary>
    /// Get token at index Idx,
    /// @returns True and the Token if there is one
    ///          False and Token=nil if index is out of bounds </summary>
    function TryGetToken(_Idx: Integer; out _Token: TPascalToken): Boolean; {$IFDEF SupportsInline} inline; {$ENDIF}
    ///<summary>
    /// Check whether the token at index Idx has the reserved type RType
    /// @param Idx is the index of the token to check
    /// @param RType is the queried reserverd type
    /// @returns True, if the token has the queried type, False otherwise
    ///          also False, if the index is out of bounds. </summary>
    function TokenAtIs(_Idx: Integer; _rType: TReservedType): Boolean; overload; {$IFDEF SupportsInline} inline; {$ENDIF}
    function TokenAtIs(_Idx: Integer; _rTypes: TReservedTypeSet): Boolean; overload;{$IFDEF SupportsInline} inline; {$ENDIF}

    function GetNextNoComment(_StartPos: Integer; out _Offset: Integer): TPascalToken; {$IFDEF SupportsInline} inline; {$ENDIF}
    function TryGetNextNoComment(_StartPos: Integer; out _Token: TPascalToken; out _Offset: Integer): Boolean; overload; {$IFDEF SupportsInline} inline; {$ENDIF}
    function TryGetNextNoComment(_StartPos: Integer; out _Token: TPascalToken): Boolean; overload; {$IFDEF SupportsInline} inline; {$ENDIF}

    function InsertBlankLines(_AtIndex, _NLines: Integer): TLineFeed;
    function AssertLineFeedAfter(_StartPos: Integer): TLineFeed; {$IFDEF SupportsInline} inline; {$ENDIF}
    procedure CheckSlashComment;
    procedure ComplexIfElse(_NTmp: Integer);
    procedure CheckShortLine;

    ///<summary>
    /// This function does the actual formatting </summary>
    procedure doExecute(_Tokens: TPascalTokenList);
    procedure HandleIf;
    procedure HandleThen;
    procedure HandleColon(_RemoveMe: Integer);
    procedure HandleElse(_NTmp: Integer);
    function DetectGenericStart(_TokenIdx: Integer): Boolean;
    procedure CheckIndent(var _NTmp: Integer; var _PrevOldNspaces: Integer);
    procedure HandleCapitalization(_CurrentToken: TPascalToken);
    function TryGetPrevLineFeed(var _TokenIdx: Integer; out _LineFeed: TLineFeed): Boolean;
  public
    class procedure Execute(_Tokens: TPascalTokenList; _Settings: TCodeFormatterSettings);
    constructor Create(_Settings: TCodeFormatterSettings);
    destructor Destroy; override;
  end;

implementation

uses
  StrUtils,
  TypInfo,
{$IFDEF GX_VER250_up}
  AnsiStrings,
{$ENDIF}
  u_dzStringUtils,
  u_dzAssertTrace;

class procedure TCodeFormatterFormatter.Execute(_Tokens: TPascalTokenList; _Settings: TCodeFormatterSettings);
var
  Formatter: TCodeFormatterFormatter;
begin
  Formatter := TCodeFormatterFormatter.Create(_Settings);
  try
    Formatter.doExecute(_Tokens);
  finally
    Formatter.Free;
  end;
end;

constructor TCodeFormatterFormatter.Create(_Settings: TCodeFormatterSettings);
begin
  inherited Create;
  FSettings := _Settings;
  FHasAligned := False;
  FPrevLine := nil;
  FStack := TCodeFormatterSegment.Create;
end;

destructor TCodeFormatterFormatter.Destroy;
begin
  FStack.Free;
  inherited;
end;

function TCodeFormatterFormatter.AlignExpression(_Idx: Integer; _Pos: Integer): TPascalToken;
var
  OldExpr: TExpression;
begin
  FHasAligned := True;
  OldExpr := TExpression(FTokens.Extract(_Idx));
  Result := TAlignExpression.Create(OldExpr, _Pos);
  FTokens.AtInsert(_Idx, Result);
  OldExpr.Free;
end;

function TCodeFormatterFormatter.DetectGenericStart(_TokenIdx: Integer): Boolean;
var
  Next: TPascalToken;
  Idx: Integer;
  Offset: Integer;
  exp: TGXUnicodeString;
  rType: TReservedType;
begin
  Result := False;

  if FCurrentRType <> rtLogOper then
    Exit; //==>

  if not FCurrentToken.GetExpression(exp) or (exp <> '<') then
    Exit; //==>

  if not IsWType(FPrevToken, wtWord) then
    Exit; //==>

  Idx := 0;
  rType := FStack.GetType(Idx);
  while rType = rtGenericStart do begin
    Inc(Idx);
    rType := FStack.GetType(Idx);
  end;
  if rType in [rtClass, rtType, rtVar, rtProcedure, rtProcDeclare] then begin
    Result := True;
    Exit; //==>
  end;

  // These were the easy cases.
  // Now we must detect whether a '<' is followed by a '>' with reasonable intermediate tokens.

  if not TryGetNextNoComment(_TokenIdx, Next, Offset) then
    Exit; //==>

  // the next token must be an identifier (= rtNothing) (correct?)
  if (Next.ReservedType <> rtNothing) or (Next.WordType <> wtWord) then
    Exit; //==>

  Idx := _TokenIdx + Offset;
  while TryGetNextNoComment(Idx, Next, Offset) do begin
    case Next.ReservedType of
      rtLogOper: begin
          if Next.GetExpression(exp) and (exp = '>') then begin
            Result := True;
            Exit; //==>
          end;
          // no '>' ? -> no Generic
          Exit; //==>
        end;
      // also allowed:
      rtNothing: begin
          // but only an identifier (correct?)
          if Next.WordType <> wtWord then
            Exit; //==>
        end;
      rtComma: begin
        // OK
        end;
      // anything else?
    else
      Exit; //==>
    end;
    Inc(Idx, Offset + 1);
  end;
end;

procedure TCodeFormatterFormatter.HandleCapitalization(_CurrentToken: TPascalToken);
var
  rType: TReservedType;
begin
  rType := _CurrentToken.ReservedType;

  if not (rType in NoReservedTypes) then
    _CurrentToken.ExpressionCase := FSettings.ReservedCase
  else if rType in StandardDirectives then
    _CurrentToken.ExpressionCase := FSettings.StandDirectivesCase
  else begin
    _CurrentToken.ExpressionCase := rfUnchanged;
    if _CurrentToken.WordType = wtWord then begin
      // todo: this shouldn't be a method of the Settings object
      FSettings.HandleCapitalization(_CurrentToken);
    end;
  end;
end;

procedure TCodeFormatterFormatter.AdjustSpacing(_CurrentToken, _PrevToken: TPascalToken; _TokenIdx: Integer);
var
  Prev2: TPascalToken;
  rType: TReservedType;
  wType: TWordType;
  Idx: Integer;
  exp: TGXUnicodeString;
begin
  if _CurrentToken = nil then
    Exit;

  { TODO -otwm : This doesn't really belong here, it has nothing to do with spacing
                 it also unnecessarily sets each token's case three times }
  HandleCapitalization(_CurrentToken);

  rType := _CurrentToken.ReservedType;
  wType := _CurrentToken.WordType;

  Assert(False, '.AdjustSpacing');
  case rType of
    rtThen, rtOf, rtElse, rtDo, rtAsm: begin
        Assert(False, '.AdjustSpacing');
        _CurrentToken.SetSpace([spBefore, spAfter], True);
      end;

    rtEnd, rtFuncDirective: begin
        Assert(False, '.AdjustSpacing');
        _CurrentToken.SetSpace([spBefore], True);
      end;

    rtIf, rtUntil, rtWhile, rtCase, rtRecord: begin
        Assert(False, '.AdjustSpacing');
        _CurrentToken.SetSpace([spAfter], True);
      end;

    rtLogOper: begin
        Assert(False, '.AdjustSpacing');
        if not _CurrentToken.GetExpression(exp) then
          raise EFormatException.Create('Programmer error: GetExpression for logical operator returned False');

        if (exp = '<') then begin
          Assert(False, '.AdjustSpacing');
          if DetectGenericStart(_TokenIdx) then begin
            Assert(False, '.AdjustSpacing');
            FStack.Push(rtGenericStart, 0);
            _CurrentToken.SetSpace([], True)
          end else begin
            Assert(False, '.AdjustSpacing');
            _CurrentToken.SetSpace(FSettings.SpaceOperators, True);
          end;
        end else if exp = '>' then begin
          Assert(False, '.AdjustSpacing');
          if FStack.HasType(rtGenericStart) then begin
            Assert(False, '.AdjustSpacing');
            _CurrentToken.SetSpace([], True);
            repeat
              Assert(False, '.AdjustSpacing');
              FLastPopResType := FStack.Pop;
            until (FLastPopResType = rtGenericStart) or FStack.IsEmpty;
          end else
            _CurrentToken.SetSpace(FSettings.SpaceOperators, True);
        end else
          _CurrentToken.SetSpace(FSettings.SpaceOperators, True);
      end;

    rtOper, rtMathOper, rtPlus, rtMinus, rtEquals: begin
        Assert(False, '.AdjustSpacing');
        _CurrentToken.SetSpace(FSettings.SpaceOperators, True);
      end;

    rtAssignOper: begin
        Assert(False, '.AdjustSpacing');
        _CurrentToken.SetSpace(FSettings.SpaceEqualOper, True);
      end;

    rtColon: begin
        Assert(False, '.AdjustSpacing');
        _CurrentToken.SetSpace(FSettings.SpaceColon, True);
      end;

    rtSemiColon: begin
        Assert(False, '.AdjustSpacing');
        _CurrentToken.SetSpace(FSettings.SpaceSemiColon, True);
      end;

    rtComma: begin
        Assert(False, '.AdjustSpacing');
        _CurrentToken.SetSpace(FSettings.SpaceComma, True);
      end;

    rtLeftBr: begin
        Assert(False, '.AdjustSpacing');
        _CurrentToken.SetSpace(FSettings.SpaceLeftBr, True);
        if _PrevToken.ReservedType = rtLeftBr then begin
          Assert(False, '.AdjustSpacing');
          _CurrentToken.SetSpace([spBefore], False);
        end;
      end;

    rtLeftHook: begin
        Assert(False, '.AdjustSpacing');
        _CurrentToken.SetSpace(FSettings.SpaceLeftHook, True);
        if _PrevToken.ReservedType = rtLeftHook then begin
          Assert(False, '.AdjustSpacing');
          _CurrentToken.SetSpace([spBefore], False);
        end;
      end;

    rtRightBr: begin
        Assert(False, '.AdjustSpacing');
        _CurrentToken.SetSpace(FSettings.SpaceRightBr, True);
      end;

    rtRightHook: begin
        Assert(False, '.AdjustSpacing');
        _CurrentToken.SetSpace(FSettings.SpaceRightHook, True);
      end;
  end;

  // todo: This doesn't belong here, it's about the case of hex numbers, not spacing
  if (wType = wtHexNumber) and FSettings.UpperNumbers then begin
    Assert(False, '.AdjustSpacing');
    _CurrentToken.SetExpressionCase(rfUpperCase);
  end;

  { delimiter between 2 words (necessary) }

  if _PrevToken = nil then begin
    Assert(False, '.AdjustSpacing');
    Exit;
  end;

  { append space after : , ; }
  if FSettings.SpaceOperators <> [] then begin
    Assert(False, '.AdjustSpacing');
    if wType in [wtString, wtFullComment, wtHalfComment, wtHalfStarComment] then begin
      Assert(False, '.AdjustSpacing');
      if not (_PrevToken.ReservedType in [rtDotDot, rtLineFeed]) then begin
        Assert(False, '.AdjustSpacing');
        _CurrentToken.SetSpace([spBefore], True);
      end;
    end;
  end;

  if rType in [rtMinus, rtPlus] then begin
    Prev2 := _PrevToken;
    Idx := 0;

    while IsRType(Prev2, [rtComment, rtLineFeed]) do begin
      Assert(False, '.AdjustSpacing');
      Inc(Idx);
      if Idx > _TokenIdx then begin
        Assert(False, '.AdjustSpacing');
        Prev2 := nil;
      end else begin
        Assert(False, '.AdjustSpacing');
        Prev2 := FTokens[_TokenIdx - Idx];
      end;
    end;

    if IsRType(Prev2, [rtOper,
        rtMathOper, rtPlus, rtMinus, rtSemiColon, rtOf,
        rtMinus, rtLogOper, rtEquals, rtAssignOper, rtLeftBr,
        rtLeftHook, rtComma, rtDefault]) then begin
      Assert(False, '.AdjustSpacing');
      _CurrentToken.SetSpace([spAfter], False); { sign operator }
    end;
  end;

  if rType = rtLeftHook then begin
    Assert(False, '.AdjustSpacing');
    if not (_PrevToken.ReservedType in [rtReserved, rtNothing, rtRightBr, rtRightHook]) then begin
      Assert(False, '.AdjustSpacing');
      _CurrentToken.SetSpace([spBefore], True);
    end;
  end;

  if _CurrentToken.Space(spBefore)
    and (_PrevToken.ReservedType in [rtLeftBr, rtLeftHook, rtLineFeed]) then begin
    Assert(False, '.AdjustSpacing');
    _CurrentToken.SetSpace([spBefore], False);
  end;

  if (_PrevToken.WordType in [wtWord, wtNumber, wtHexNumber, wtString])
    and (wType in [wtWord, wtNumber, wtHexNumber]) then begin
    Assert(False, '.AdjustSpacing');
    _CurrentToken.SetSpace([spBefore], True);
  end;

  if _CurrentToken.Space(spBefore) and _PrevToken.Space(spAfter) then begin
    Assert(False, '.AdjustSpacing');
    _PrevToken.SetSpace([spAfter], False); { avoid double spaces }
  end;
end;

function TCodeFormatterFormatter.TokenAtIs(_Idx: Integer; _rType: TReservedType): Boolean;
var
  Token: TPascalToken;
begin
  Result := TryGetToken(_Idx, Token);
  if Result then
    Result := (Token.ReservedType = _rType);
end;

function TCodeFormatterFormatter.TokenAtIs(_Idx: Integer; _rTypes: TReservedTypeSet): Boolean;
var
  Token: TPascalToken;
begin
  Result := TryGetToken(_Idx, Token);
  if Result then
    Result := (Token.ReservedType in _rTypes);
end;

function TCodeFormatterFormatter.GetToken(_Idx: Integer): TPascalToken;
begin
  TryGetToken(_Idx, Result);
end;

function TCodeFormatterFormatter.TryGetToken(_Idx: Integer; out _Token: TPascalToken): Boolean;
begin
  Result := (_Idx >= 0) and (_Idx < FTokens.Count);
  if Result then
    _Token := FTokens[_Idx]
  else
    _Token := nil;
end;

function TCodeFormatterFormatter.GetNextNoComment(_StartPos: Integer; out _Offset: Integer): TPascalToken;
begin
  if not TryGetNextNoComment(_StartPos, Result, _Offset) then
    Result := nil;
end;

function TCodeFormatterFormatter.TryGetNextNoComment(_StartPos: Integer; out _Token: TPascalToken; out _Offset: Integer): Boolean;
begin
  _Offset := 0;

  repeat
    Inc(_Offset);
    Result := TryGetToken(_StartPos + _Offset, _Token);
  until not Result or (_Token.ReservedType <> rtComment);
end;

function TCodeFormatterFormatter.TryGetNextNoComment(_StartPos: Integer; out _Token: TPascalToken): Boolean;
var
  Offset: Integer;
begin
  Result := TryGetNextNoComment(_StartPos, _Token, Offset);
end;

function TCodeFormatterFormatter.InsertBlankLines(_AtIndex, _NLines: Integer): TLineFeed;
var
  LineIdx: Integer;
  NextToken: TPascalToken;
begin
  Result := FPrevLine;

  for LineIdx := 0 to _NLines - 1 do begin //FI:W528
    Result := TLineFeed.Create(0, FSettings.SpacePerIndent);
    Result.SetIndent(FStack.nIndent);
    NextToken := GetToken(_AtIndex);

    { TODO -otwm -ccheck : is the if statement necessary? }
    if NextToken.Space(spBefore) then
      NextToken.SetSpace([spBefore], False);

    FTokens.AtInsert(_AtIndex, Result);
    AdjustSpacing(NextToken, Result, _AtIndex);
  end;

  if _AtIndex <= FTokenIdx then
    Inc(FTokenIdx, _NLines);
end;

function TCodeFormatterFormatter.AssertLineFeedAfter(_StartPos: Integer): TLineFeed;
var
  Next: TPascalToken;
  Offset: Integer;
begin
  if TryGetNextNoComment(_StartPos, Next, Offset) and (Next.ReservedType <> rtLineFeed) then
    Result := InsertBlankLines(_StartPos + Offset, 1)
  else
    Result := FPrevLine;
end;

procedure TCodeFormatterFormatter.DecPrevLineIndent;
begin
  if FPrevLine <> nil then
    FPrevLine.IncIndent(-1);
end;

function TCodeFormatterFormatter.TryGetPrevLineFeed(var _TokenIdx: Integer;
  out _LineFeed: TLineFeed): Boolean;
var
  Token: TPascalToken;
begin
  repeat
    Dec(_TokenIdx);
    if not TryGetToken(_TokenIdx, Token) then begin
      Result := False;
      Exit; //==>
    end;
  until Token.ReservedType = rtLineFeed;
  _LineFeed := Token as TLineFeed;
  Result := True;
end;

procedure TCodeFormatterFormatter.IndentProcedureComment;
var
  CommentLFs: array of TLineFeed;
  Idx: Integer;
  ThisLineStart: TLineFeed;
  DesiredNoOfSpaces: Integer;
  Len: Integer;
  LineFeed: TLineFeed;
  i: Integer;
  Diff: Integer;
begin
  Idx := FTokenIdx;
  if not TryGetPrevLineFeed(Idx, ThisLineStart) then
    Exit; //==>

  if not TokenAtIs(Idx - 1, rtComment) then begin
    // no comments before the procedure
    Exit; //==>
  end;

  // todo: This can be implemented more efficiently because we know that
  //       Idx-2n-1 is a comment and Idx-2n must be the line feed in front of that comment
  //       until we hit a non-comment at Idx-2n-1.
  //       -> No, we don't know that. There can be single line feeds in between that break
  //          the 2n rule like this:
  // { first line            <- LF + comment
  //                         <- single LF
  //   third line }          <- LF + comment
  // procedure bla;
  //
  // get a list of all line feeds in front of comments before the procedure
  while TryGetPrevLineFeed(Idx, LineFeed) and TokenAtIs(Idx + 1, [rtComment, rtLineFeed]) do begin
    Len := Length(CommentLFs);
    SetLength(CommentLFs, Len + 1);
    CommentLFs[Len] := LineFeed;
  end;

  // We assume that the comments have already been arranged correctly to match
  // the first comment. So we determine the adjustment for that first comment
  // and apply it to all comment lines.

  DesiredNoOfSpaces := ThisLineStart.NoOfSpaces;
  Len := Length(CommentLFs);
  Assert(Len > 0);
  LineFeed := CommentLFs[Len - 1];
  Diff := LineFeed.NoOfSpaces - DesiredNoOfSpaces;
  LineFeed.NoOfSpaces := DesiredNoOfSpaces;
  for i := 0 to Len - 2 do begin
    LineFeed := CommentLFs[i];
    LineFeed.NoOfSpaces := LineFeed.NoOfSpaces - Diff;
  end;
end;

procedure TCodeFormatterFormatter.SetPrevLineIndent(_Additional: Integer);
begin
  if FPrevLine <> nil then
    FPrevLine.SetIndent(FStack.nIndent + _Additional + FStack.ProcLevel);
end;

function TCodeFormatterFormatter.NoBeginTryIndent(_rType: TReservedType): Boolean;
begin
  Result := not (
    (FSettings.IndentBegin and (_rType = rtBegin))
    or (FSettings.IndentTry and (_rType = rtTry))
    )
    and (FStack.GetTopType in [rtDo, rtThen, rtIfElse]);
end;

procedure TCodeFormatterFormatter.UppercaseCompilerDirective(_Token: TPascalToken);
var
  Idx: Integer;
  s: TGXUnicodeString;
begin
  _Token.GetExpression(s);
  Idx := 2;
  while (Idx < Length(s)) and (s[Idx] <> Space) and (s[Idx] <> Tab) do begin
    s[Idx] := UpCase(s[Idx]);
    Inc(Idx);
  end;

  _Token.SetExpression(s);
end;

function TCodeFormatterFormatter.IsRType(_Token: TPascalToken; _rType: TReservedType): Boolean;
begin
  Result := Assigned(_Token) and (_Token.ReservedType = _rType);
end;

function TCodeFormatterFormatter.IsRType(_Token: TPascalToken;
  _rTypeSet: TReservedTypeSet): Boolean;
begin
  Result := Assigned(_Token) and (_Token.ReservedType in _rTypeSet);
end;

function TCodeFormatterFormatter.IsWType(_Token: TPascalToken; _wType: TWordType): Boolean;
begin
  Result := Assigned(_Token) and (_Token.WordType = _wType);
end;

procedure TCodeFormatterFormatter.CheckBlankLinesAroundProc;
var
  k: Integer;
  Prev2: TPascalToken;
begin
  if FPrevToken = nil then
    Exit; //==>

  if FPrevToken.ReservedType = rtClass then begin
    // class procedure / function, skip "class"
    k := 2;
    Prev2 := GetToken(FTokenIdx - 2);
  end else begin
    // just procedure / function
    k := 1;
    Prev2 := FPrevToken;
  end;

  if not IsRType(Prev2, rtLineFeed) then begin
    // no line feed at all -> add two for an empty line
    // todo --fixme: This doesn't work. At least not for ...
    //
    // {bla}procedure blub;
    //
    // ... which should be formatted as ...
    //
    // <empty line>
    // {bla}
    // procedure blub;
    //
    // ... Or should it be ...
    //
    // <empty line>
    // {bla}procedure blub;
    //
    // ... ?
    //
    // And how should we handle ...
    //
    // {blub}
    // {bla}procedure blub;
    //
    // ... ?
    FPrevLine := InsertBlankLines(FTokenIdx - k, 2);
    FPrevToken := FPrevLine;
  end else begin
    // Thats the line feed at the begin of this line.
    // Now check for another one before that
    Inc(k);
    if not TryGetToken(FTokenIdx - k, Prev2) or (Prev2.ReservedType = rtLineFeed) then begin
      // There is no previous token or there is already a second line feed, we are done.
      Exit; //==>
    end;

    if (Prev2.ReservedType = rtComment) and TokenAtIs(FTokenIdx - k - 1, rtLineFeed) then begin
      // There is a comment before the procedure. Check for more comments before that.
      Inc(k, 2);
      while TryGetToken(FTokenIdx - k, Prev2) and (Prev2.ReservedType = rtComment)
        and TokenAtIs(FTokenIdx - k - 1, rtLineFeed) do begin
        Inc(k, 2);
      end;
      // We found the line feed before the comment(s).
      // Is there another one just before?
      if not TokenAtIs(FTokenIdx - k, rtLineFeed) then begin
        // no -> insert one
        InsertBlankLines(FTokenIdx - k + 1, 1);
      end;
    end else begin
      // only one line feed, add one for an empty line
      FPrevLine := InsertBlankLines(FTokenIdx - k + 1, 1);
      FPrevToken := FPrevLine;
    end;
  end;
end;

//procedure TCodeFormatterFormatter.PutCommentBefore(const _Comment: TGXUnicodeString);
//var
//  j: Integer;
//  P: TPascalToken;
//  s: TGXUnicodeString;
//begin
//  j := FTokenIdx - 2;
//  P := GetToken(j);
//
//  s := _Comment;
//
//  if P.ReservedType = rtComment then
//    P.SetExpression(s)
//  else begin
//    P := TExpression.Create(wtWord, s);
//    P.SetReservedType(rtComment);
//    FTokens.AtInsert(FTokenIdx, P);
//    Inc(FTokenIdx);
//    P := TLineFeed.Create(0, FSettings.SpacePerIndent);
//    TLineFeed(P).SetIndent(FStack.nIndent);
//    FTokens.AtInsert(FTokenIdx, P);
//    Inc(FTokenIdx);
//  end;
//end;

// When we enter this method FCurrentToken is 'asm' and FCurrentRType is rtAsm
procedure TCodeFormatterFormatter.FormatAsm(_NTmp: Integer);
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
      FPrevLine.NoOfSpaces := FPrevLine.OldNoOfSpaces;
    end;

    AdjustSpacing(FCurrentToken, FPrevToken, FTokenIdx);
    Inc(FTokenIdx);
    FPrevToken := FCurrentToken;
    FCurrentToken := GetToken(FTokenIdx);
  end;

  if FTokenIdx < FTokens.Count then
    SetPrevLineIndent(_NTmp);

  Dec(FTokenIdx);
end;

procedure TCodeFormatterFormatter.CheckSlashComment;
var
  Token: TPascalToken;
  PrevPasWord: TPascalToken;
  Expression: TGXUnicodeString;
  PrevExpression: TGXUnicodeString;
  i: Integer;
begin
  if TryGetToken(FTokenIdx - 1, FPrevToken) and (FPrevToken.ReservedType = rtComment)
    and FPrevToken.GetExpression(PrevExpression) and (PrevExpression[1] = '/') then begin
    // fix for situation with a // comment on prev line: begin becomes part of the comment
    if FPrevToken.ChangeComment('{') then begin
      FPrevToken.SetSpace([spAfter], True);
    end else begin
      i := 0;
      Token := nil;

      repeat
        PrevPasWord := Token;
        Token := GetToken(FTokenIdx + i);
        Inc(i);
      until not IsRType(Token, rtLineFeed);

      Dec(i);
      if (PrevPasWord.ReservedType = rtComment)
        and PrevPasWord.GetExpression(Expression)
        and (Expression[1] = '/') then begin
        FPrevToken.SetExpression('{' + Copy(PrevExpression, 2) + '}');
        Exit;
      end else
        FTokens.Extract(FTokenIdx - 1);

      FTokens.AtInsert(FTokenIdx + i, FPrevToken);
      FPrevToken := GetToken(FTokenIdx - 1);
      AdjustSpacing(FPrevToken, GetToken(FTokenIdx - 2), FTokenIdx - 1);
      FCurrentToken := GetToken(FTokenIdx);
    end;
  end;

  FPrevLine := FPrevPrevLine;
end;

procedure TCodeFormatterFormatter.ComplexIfElse(_NTmp: Integer);
begin
  while not FStack.IsEmpty and (FLastPopResType <> rtThen) do begin
    FLastPopResType := FStack.Pop;
    if FLastPopResType = rtIfElse then
      ComplexIfElse(_NTmp);
  end;

  SetPrevLineIndent(_NTmp);
end;

procedure TCodeFormatterFormatter.CheckShortLine;
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
  Offset: Integer;
begin { CheckShortLine }
  Offset := 1;
  Token := GetToken(FTokenIdx + Offset);
  if TokenRType <> rtLineFeed then
    Exit;

  while not ((TokenRType in [rtSemiColon, rtBegin, rtElse, rtDo, rtWhile, rtOn, rtThen, rtCase])
    or ((Offset > 1) and (Token.ReservedType = rtLineFeed))) do begin
    Inc(Offset);
    Token := GetToken(FTokenIdx + Offset);
  end;

  if TokenRType = rtSemiColon then
    FTokens.Extract(FTokenIdx + 1).Free;
end;

procedure TCodeFormatterFormatter.HandleIf;
begin
  if FSettings.FeedAfterThen and not FSettings.FeedElseIf
    and (FStack.GetTopType = rtIfElse) and (FPrevToken = FPrevLine) then begin
    FTokens.Extract(FTokenIdx - 1).Free;
    Dec(FTokenIdx);
    CheckSlashComment;
  end else begin
    if FSettings.FeedElseIf and (FPrevToken <> FPrevLine) then begin
      FPrevLine := AssertLineFeedAfter(FTokenIdx - 1);
      FPrevToken := FPrevLine;
    end;
  end;

  if IsRType(FPrevToken, rtElse)
    or (FSettings.NoIndentElseIf and (FStack.GetTopType = rtIfElse)) then begin
    FStack.Pop;
    if FStack.GetTopType = rtThen then
      FStack.Pop;
    FWrapIndent := True;
    FStack.Push(rtIfElse, 0);
  end else
    FStack.Push(rtIf, 0);
end;

procedure TCodeFormatterFormatter.HandleThen;
begin
  if FStack.GetTopType in [rtIf, rtIfElse] then begin
    FWrapIndent := False;
    FLastPopResType := FStack.Pop;
    if FSettings.NoFeedBeforeThen and (FPrevToken = FPrevLine)
      and (GetToken(FTokenIdx - 1).ReservedType <> rtComment) then begin
      FTokens.Extract(FTokenIdx - 1).Free;
      Dec(FTokenIdx);
      CheckSlashComment;
    end;
    if FSettings.FeedAfterThen then begin
      if AssertLineFeedAfter(FTokenIdx) <> FPrevLine then begin
        if (FLastPopResType = rtIf) and FSettings.ExceptSingle then
          CheckShortLine;
      end;
    end;
    FStack.Push(rtThen, 1);
  end;
end;

procedure TCodeFormatterFormatter.HandleColon(_RemoveMe: Integer);
begin
  case FStack.GetTopType of
    rtOf: begin
        FStack.Push(FCurrentRType, 1);
        if FSettings.FeedAfterThen then begin
          if (GetNextNoComment(FTokenIdx, _RemoveMe).ReservedType = rtBegin)
            and (AssertLineFeedAfter(FTokenIdx) <> FPrevLine) then
            CheckShortLine;
        end;
        FWrapIndent := False;
      end;

    rtClassDecl: begin
        FStack.Pop;
        FStack.Push(rtClass, 1);
      end;

    rtVar:
      if FSettings.AlignVar then
        FCurrentToken := AlignExpression(FTokenIdx, FSettings.AlignVarPos);

    rtProcedure, rtProcDeclare:
      ; // do nothing
  else
    // label????
    FWrapIndent := False;
  end;
end;

procedure TCodeFormatterFormatter.HandleElse(_NTmp: Integer);
var
  Next: TPascalToken;
begin
  FLastPopResType := rtNothing;

  while not FStack.IsEmpty and not (FStack.GetTopType in [rtThen, rtOf, rtTry]) do
    FLastPopResType := FStack.Pop;

  if FLastPopResType = rtIfElse then
    ComplexIfElse(_NTmp);

  if (FSettings.FeedRoundBegin = Hanging)
    and (FPrevToken <> nil)
    and TokenAtIs(FTokenIdx - 1, rtLineFeed)
    and TokenAtIs(FTokenIdx - 2, rtEnd) then begin
    FTokens.Extract(FTokenIdx - 1).Free;
    Dec(FTokenIdx);
    FPrevLine := nil;
    FPrevToken := FPrevLine;
  end;

  if FSettings.FeedAfterThen then begin
    if (FPrevToken <> nil)
      and ((FSettings.FeedRoundBegin <> Hanging) or not TokenAtIs(FTokenIdx - 1, rtEnd))
      and not TokenAtIs(FTokenIdx - 1, rtLineFeed) then begin
      FPrevLine := AssertLineFeedAfter(FTokenIdx - 1);
      FPrevToken := FPrevLine;
    end;

    if TryGetNextNoComment(FTokenIdx, Next) and (Next.ReservedType <> rtIf) then
      AssertLineFeedAfter(FTokenIdx);
  end;

  FStack.nIndent := FStack.GetTopIndent;
  if FPrevToken = FPrevLine then
    SetPrevLineIndent(_NTmp);

  if FSettings.FeedBeforeElse then begin
    if not TokenAtIs(FTokenIdx - 1, rtLineFeed) then begin
      FPrevLine := AssertLineFeedAfter(FTokenIdx - 1);
      FPrevToken := FPrevLine;
    end;
  end;

  if FSettings.IndentTryElse and (FStack.GetTopType = rtTry) then begin
    FStack.nIndent := FStack.nIndent + 1;
    SetPrevLineIndent(_NTmp);
  end else if FSettings.IndentCaseElse and (FStack.GetTopType = rtOf) then begin
    FStack.nIndent := FStack.nIndent + 1;
    SetPrevLineIndent(_NTmp);
  end;

  if FStack.GetTopType = rtThen then
    FStack.Push(rtIfElse, 1)
  else
    FStack.Push(rtElse, 1);

  FWrapIndent := False;
end;

procedure TCodeFormatterFormatter.CheckIndent(var _NTmp: Integer; var _PrevOldNspaces: Integer);
var
  RemoveMe: Integer;
  Next: TPascalToken;
  TempWordIdx: Integer;
  Prev1: TPascalToken;
  FunctDeclare, IsDelegate, NoBlankLine: Boolean;
  FeedRound: TFeedBegin;
  wType: TWordType;
  exp: TGXUnicodeString;
begin
  if FCurrentToken = nil then
    Exit;

  FCurrentRType := FCurrentToken.ReservedType;
  wType := FCurrentToken.WordType;

  // This handles the case where a reserved word was used as the name of a class member.
  // (Is that even allowed?)
  if (FCurrentRType in [rtWhile, rtEnd, rtRepeat, rtBegin, rtUses, rtTry,
      rtProgram, rtType, rtVar, rtIf, rtThen, rtElse] + StandardDirectives)
    and IsRType(FPrevToken, rtDot) then begin
    Assert(False, '.CheckIndent: SetReservedType to rtNothing');
    FCurrentToken.SetReservedType(rtNothing);
    FCurrentRType := rtNothing;
  end;

  { SetSpacing; }
  case FCurrentRType of
    rtIf:
      HandleIf;

    rtThen:
      HandleThen;

    rtColon:
      HandleColon(RemoveMe);

    rtElse:
      HandleElse(_NTmp);

    rtRepeat, rtRecord: begin
        FStack.Push(FCurrentRType, 1);
        FWrapIndent := False;
      end;

    rtClass: begin
        if not (TryGetNextNoComment(FTokenIdx, Next)
          and (Next.ReservedType in [rtProcedure, rtProcDeclare, rtOf, rtVar])) then begin
          // not a "class function" or "class of" declaration
          FWrapIndent := False;
          FStack.Push(rtClassDecl, 1);
        end else begin
          // first assume that it is a class declaration the first procedure replaces it with rtClass
          FCurrentToken.SetSpace([spAfter], True);
        end;
      end;

    rtUntil: begin
        repeat
          FLastPopResType := FStack.Pop;
        until (FLastPopResType = rtRepeat) or FStack.IsEmpty;
        SetPrevLineIndent(_NTmp);
      end;

    rtLeftBr: begin
        Assert(False, '.CheckIndent: LeftBr');
        if (FStack.GetTopType = rtLeftBr) then begin
          Assert(False, '.CheckIndent: Top is LeftBr');
          FStack.Push(FCurrentRType, 0)
        end else begin
          Assert(False, '.CheckIndent: Top is not Leftbr');
          FOldWrapIndent := FWrapIndent;
          if (FStack.ProcLevel <= 0) or (FStack.GetTopType <> rtProcedure) then begin
            Assert(False, '.CheckIndent');
            // not very clean
            FStack.Push(FCurrentRType, 1);
          end else begin
            Assert(False, '.CheckIndent');
            RemoveMe := 1;
            while (FTokenIdx > RemoveMe) and (TryGetToken(FTokenIdx - RemoveMe, Next)
              and (Next.ReservedType in [rtDot, rtNothing])) do begin
              Assert(False, '.CheckIndent');
              Inc(RemoveMe);
            end;
            if IsRType(Next, rtProcedure) then begin
              Assert(False, '.CheckIndent');
              FStack.Push(FCurrentRType, 0);
            end else begin
              Assert(False, '.CheckIndent');
              FStack.Push(FCurrentRType, 1);
            end;
          end;
          Assert(False, '.CheckIndent');
          FWrapIndent := False;
        end;
      end;

    rtWhile: begin
       // Helper For
        if not IsRType(FPrevToken, rtReserved) then
          FStack.Push(FCurrentRType, 0);
      end;

    rtLeftHook: begin
        // left hook = '['
        Assert(False, '.CheckIndent: LeftHook');
        if IsWType(FPrevToken, wtWord) and not IsRType(FPrevToken, rtOper) then begin
          Assert(False, '.CheckIndent');
          FStack.Push(FCurrentRType, 0);
        end else begin
          Assert(False, '.CheckIndent');
          FWrapIndent := False;
          Assert(False, '.CheckIndent');
          FStack.Push(FCurrentRType, 1);
        end;
      end;

    rtOn: begin
        FStack.Push(FCurrentRType, 0);
      end;

    rtRightBr: begin
        Assert(False, '.CheckIndent: RightBr');
        repeat
          Assert(False, '.CheckIndent');
          FLastPopResType := FStack.Pop;
        until (FLastPopResType = rtLeftBr) or FStack.IsEmpty;

        if FStack.GetTopType <> rtLeftBr then begin
          Assert(False, '.CheckIndent');
          FWrapIndent := FOldWrapIndent;
        end;
      end;

    rtRightHook: begin
        // right hook = ']'
        Assert(False, '.CheckIndent: RightHook');
        repeat
          Assert(False, '.CheckIndent');
          FLastPopResType := FStack.Pop;
        until (FLastPopResType = rtLeftHook) or FStack.IsEmpty;

        Assert(False, '.CheckIndent');
        if FStack.GetTopType = rtClassDecl then begin
          // Interface GUID
          Assert(False, '.CheckIndent');
          FWrapIndent := False;
//          end else if FStack.GetTopType = rtLeftHook then begin
//            Assert(False, '.CheckIndent');
//            FWrapIndent := FOldHookWrapIndent;
        end;
      end;

    rtExcept: begin
        while not FStack.IsEmpty and (FStack.GetTopType <> rtTry) do
          FStack.Pop;

        FStack.nIndent := FStack.GetTopIndent;
        SetPrevLineIndent(_NTmp);
        FStack.nIndent := FStack.nIndent + 1;
        FWrapIndent := False;
      end;

    rtVisibility:
      if FStack.GetTopType in [rtClass, rtClassDecl, rtRecord] then begin
        if IsRType(FPrevToken, rtLineFeed) then begin
          DecPrevLineIndent;
          FWrapIndent := False;
        end;
      end else if (FStack.GetTopType in [rtVar, rtType]) and (FStack.GetType(1) in [rtClass, rtClassDecl, rtRecord]) then begin
        FStack.Pop;
        DecPrevLineIndent;
        DecPrevLineIndent;
        FWrapIndent := False;
      end else
        FCurrentToken.SetReservedType(rtNothing);

    rtOf: begin
        case FStack.GetTopType of
          rtCase: begin
              FStack.Push(FCurrentRType, 1);
              if FSettings.FeedAfterThen then
                AssertLineFeedAfter(FTokenIdx);
              FWrapIndent := False;
            end;
          rtRecord:
            FWrapIndent := False;
        end;
      end;

    rtLineFeed: begin
        Assert(False, '.CheckIndent: LineFeed');
        if FStack.IsEmpty then begin
          Assert(False, '.CheckIndent: setting WrapIndent to False');
          FWrapIndent := False;
//        end else if FStack.GetTopType in [rtIf, rtIfElse] then begin
        end else if FStack.GetTopType in [rtIf] then begin
          Assert(False, '.CheckIndent: setting WrapIndent to True');
          FWrapIndent := True;
        end;

        if FSettings.RemoveDoubleBlank and (FTokenIdx >= 2) and (FPrevToken <> nil)
          and (FPrevToken = FPrevLine) and (FTokens[FTokenIdx - 2] = FPrevPrevLine) then begin
          Assert(False, '.CheckIndent: remove double blank line');
          FTokens.Extract(FTokenIdx - 2).Free;
          Dec(FTokenIdx);
        end;

        if TryGetNextNoComment(FTokenIdx, Next) then begin
          if Next.ReservedType in [rtElse, rtIfElse, rtBegin, rtEnd, rtUntil, rtExcept] then begin
            Assert(False, '.CheckIndent: setting WrapIndent to False');
            FWrapIndent := False;
          end;

          if FWrapIndent and not FSettings.NoIndentVarDecl then begin
            Assert(False, '.CheckIndent: setting NTmp to 1');
            _NTmp := 1;
          end else begin
            Assert(False, '.CheckIndent: setting NTmp to 0');
            _NTmp := 0;
          end;

          if Next.ReservedType in [rtLineFeed] then begin
            Assert(False, '.CheckIndent: setting WrapIndent to False');
            FWrapIndent := False;
          end else if FStack.GetTopType in [rtUses, rtLeftBr] then begin
            Assert(False, '.CheckIndent: setting WrapIndent to False');
            FWrapIndent := False;
          end else if FStack.GetTopType in [rtLeftHook] then begin
            Assert(False, '.CheckIndent: setting WrapIndent to False');
            FWrapIndent := False;
          end else begin
            Assert(False, '.CheckIndent: setting WrapIndent to True');
            FWrapIndent := True;
          end;
        end;

        FPrevPrevLine := FPrevLine;
        FPrevLine := TLineFeed(FCurrentToken);
        SetPrevLineIndent(_NTmp);
      end;

    rtAsm: begin
        FormatAsm(_NTmp);
        Exit;
      end;

    rtComma: begin
        if FStack.GetTopType = rtUses then begin
          if (FSettings.NoIndentUsesComma) and (FPrevToken is TLineFeed) then
            SetPrevLineIndent(-1)
            { TODO -cfixme : The options NoIndentUsesComma and FeedEachUnit don't work well
              with each other so for now we ignore FeedEachUnit if NoIndentUsesComma is true. }
          else if FSettings.FeedEachUnit then begin
            Next := GetNextNoComment(FTokenIdx, RemoveMe);
            if Next.ReservedType <> rtLineFeed then
              AssertLineFeedAfter(FTokenIdx);
          end;
        end;
      end;

    rtProgram, rtUses, rtInitialization:
      if FStack.GetTopType <> rtLeftBr then begin
        Next := GetNextNoComment(FTokenIdx, RemoveMe);

        if (FCurrentRType = rtUses) and (FStack.GetTopType in [rtProcedure, rtProcDeclare, rtClass]) then
          FCurrentToken.SetReservedType(rtNothing)
        else begin
          DecPrevLineIndent;
          FStack.Clear;
          FStack.Push(FCurrentRType, 1);
          FWrapIndent := False;
        end;
      end;

    rtAbsolute:
      if not (FStack.GetTopType in [rtVar, rtType]) then begin
        // We are not in a type or var declaration, so it's an identifier
        // todo: is rtType really correct here?
        FCurrentToken.SetReservedType(rtNothing);
      end else if TryGetNextNoComment(FTokenIdx, Next) and (Next.ReservedType = rtColon) then begin
        // When followed by a ':', it's an identifier
        DecPrevLineIndent;
        FCurrentToken.SetReservedType(rtNothing);
      end;

    rtFuncDirective, rtDefault, rtPropertyAccess: begin
        Assert(False, '.CheckIndent: rtFuncDirective');
        if FPrevToken.ReservedType in [rtProcedure, rtProcDeclare, rtDot] then begin
          // the previous token was 'procedure' or '.' -> it's an identifier
          // todo: What if the previous token was a line feed and the one before that was
          //       'procedure' or '.' or if there was a comment in between ?
          FCurrentToken.SetReservedType(rtNothing);
        end else if not (FStack.GetTopType in [rtProcedure, rtProcDeclare, rtClass]) then begin
          // we are not in a procedure or class declaration -> it's an identifier
          FCurrentToken.SetReservedType(rtNothing);
        end else begin
          Next := GetNextNoComment(FTokenIdx, RemoveMe);
          if Next.ReservedType = rtColon then begin
            // followed by ':' -> Its an identifier
            FCurrentToken.SetReservedType(rtNothing);
          end;
        end;
      end;

    rtForward: begin
        Assert(False, '.CheckIndent: rtForward');
        if FStack.GetTopType in [rtProcedure, rtProcDeclare] then begin
          // todo: This fails because rtProcDeclare is removed from the stack as soon as
          //       a semicolon is encountered.
          FStack.Pop;
        end else begin
          // we are not in a procedure or class declaration -> it's an identifier
          FCurrentToken.SetReservedType(rtNothing);
        end;
      end;

    rtProcedure: begin
        Assert(False, '.CheckIndent: rtProcedure');
        if FStack.GetTopType in [rtClassDecl, rtRecord] then begin
          Assert(False, '.CheckIndent: Top type is Class or Record');
          FStack.Pop;
          FStack.Push(rtClass, 1);
        end else if (FStack.GetTopType in [rtVar, rtType])
          and (FStack.GetType(1) in [rtClass, rtClassDecl, rtRecord])
          and (FPrevToken.ReservedType <> rtEquals) then begin
          // There was a nested class/record declaration that ended
          Assert(False, '.CheckIndent: Nested Class or Record ended');
          FStack.Pop;
          FStack.Pop;
          FStack.Push(rtClass, 1);
          DecPrevLineIndent;
        end;

        Prev1 := FPrevToken;
        TempWordIdx := FTokenIdx;
        if Prev1 <> nil then begin
          Assert(False, '.CheckIndent: Prev1 <> nil');
          while (TempWordIdx > 0) and (Prev1.ReservedType in [rtComment, rtLineFeed]) do begin
            Assert(False, '.CheckIndent: in while');
            Dec(TempWordIdx);
            Prev1 := FTokens[TempWordIdx];
          end;

          FunctDeclare := IsRType(Prev1, [rtEquals, rtColon, rtComma, rtLeftBr]);
        end else begin
          Assert(False, '.CheckIndent: Prev1 = nil');
          FunctDeclare := False;
        end;

        NoBlankLine := False;
        IsDelegate := False;

        if not FunctDeclare then begin
          Assert(False, '.CheckIndent: FunctDeclare = False');
          RemoveMe := 0;
          repeat
            Assert(False, '.CheckIndent: in repeat');
            Inc(RemoveMe);
            if TryGetToken(FTokenIdx + RemoveMe, Next) then
              if Next.ReservedType = rtLeftBr then
                repeat
                  Inc(RemoveMe);
                until not TryGetToken(FTokenIdx + RemoveMe, Next) or (Next.ReservedType = rtRightBr);
          until (Next = nil) or (Next.ReservedType in [rtSemiColon, rtBegin]);

          if Next <> nil then begin
            if Next.ReservedType = rtBegin then begin
              // Begin before a SemiColon, presume that is a anonymous delegate...
              Assert(False, '.CheckIndent: Next.ReservedType = rtBegin');
              IsDelegate := True;
              Next.AddOption(toFeedNewLine); // Force NewLine Feed!
            end;

            Assert(False, '.CheckIndent: Next <> nil');
            repeat
              Assert(False, '.CheckIndent: in repeat');
              Inc(RemoveMe);
            until not TryGetToken(FTokenIdx + RemoveMe, Next) or not (Next.ReservedType in [rtLineFeed, rtComment]);

            if IsRType(Next, rtForward) then begin
              Assert(False, '.CheckIndent: Next = rtForward');
              NoBlankLine := True;
            end;
          end;
        end;

        if not (FunctDeclare or FIsInInterfacePart or (FStack.GetTopType = rtClass)) then begin
          Assert(False, '.CheckIndent: not (FunctDeclare or InInterface or (TopType = rtClass))');
          if not FStack.HasType(rtProcedure) then begin
            Assert(False, '.CheckIndent: Stack.HasType(rtProcedure) = False');
            if not IsDelegate then begin
              Assert(False, '.CheckIndent: not IsDelegate');

              if FStack.nIndent > 0 then begin
                Assert(False, '.CheckIndent: Stack.nIndent > 0');
                FStack.nIndent := 0;
                SetPrevLineIndent(_NTmp);
              end;

              FStack.ProcLevel := 0;
              if FSettings.BlankProc and not NoBlankLine then begin
                Assert(False, '.CheckIndent: Blank Lines Around Proc');
                CheckBlankLinesAroundProc;
              end;
            end;
          end else begin
            Assert(False, '.CheckIndent: Stack.HasType(rtProcedure) = True');
            if FSettings.BlankSubProc and not NoBlankLine then begin
              Assert(False, '.CheckIndent: Blanc Lines Around Sub Proc');
              CheckBlankLinesAroundProc;
            end;

            FStack.ProcLevel := FStack.ProcLevel + 1;

            if FStack.nIndent = 0 then begin
              Assert(False, '.CheckIndent: Stack.nIndent=0');
              SetPrevLineIndent(_NTmp);
              FStack.nIndent := FStack.nIndent + 1;
            end;
          end;

          FStack.Push(rtProcedure, 0);
        end else begin
          Assert(False, '.CheckIndent: (FunctDeclare or InInterface or (TopType = rtClass))');
          // Array of Procedure, Reference To Function...
          if (FStack.GetTopType = rtType) and IsRType(Prev1, [rtOf, rtOper, rtComma]) then begin
            Assert(False, '.CheckIndent: type declaration');
            // SetPrevLineIndent(NTmp);
            //
            // FStack.ProcLevel := FStack.ProcLevel + 1;
            ////              FStack.Push(FCurrentRType  , 1);
            //
          end else begin
            Assert(False, '.CheckIndent: no type declaration');
            if (not FunctDeclare) and (FStack.GetTopType <> rtClass) then begin
              Assert(False, '.CheckIndent: TopType <> rtClass');
              FStack.nIndent := 0;
              SetPrevLineIndent(_NTmp);
              if FSettings.IndentComments then
                IndentProcedureComment;
            end;

            FStack.Push(rtProcDeclare, 0);
          end;
        end;
      end;

    rtInterface: begin
        if IsRType(FPrevToken, rtEquals) then begin
          // declaration of a OLE object: IClass = interface ['GUID-goes-here']
          FStack.Push(rtClassDecl, 1);
        end else begin
          FIsInInterfacePart := True;
          DecPrevLineIndent;
        end;

        FWrapIndent := False;
      end;

    rtImplementation: begin
        FStack.Clear;
        FIsInInterfacePart := False;
        FWrapIndent := False;
        { DecPrevIndent; }
        { nIndent := 0; }
        SetPrevLineIndent(_NTmp);
      end;

    rtBegin, rtTry: begin
        while FStack.GetTopType in [rtVar, rtType] do
          FStack.Pop;

        if FStack.GetTopType in [rtProcedure, rtProgram] then
          FStack.Pop;

        if FStack.IsEmpty then
          FStack.nIndent := 0;

        if NoBeginTryIndent(FCurrentRType) then
          FStack.nIndent := FStack.nIndent - 1;

        case FCurrentRType of
          rtBegin:
            if FCurrentToken.HasOption(toFeedNewLine) then
              FeedRound := NewLine
            else
              FeedRound := FSettings.FeedRoundBegin;
          rtTry:
            FeedRound := FSettings.FeedRoundTry;
        else
          FeedRound := Unchanged;
        end;

        case FeedRound of
          Hanging: begin
              if (FStack.GetTopType in [rtDo, rtThen, rtIfElse, rtElse, rtColon])
                and (FPrevToken <> nil) and (GetToken(FTokenIdx - 1) = FPrevLine) then begin
                FTokens.Extract(FTokenIdx - 1).Free;
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
          SetPrevLineIndent(_NTmp);
          DecPrevLineIndent;
        end;

        FWrapIndent := False;
      end;

    rtEquals:
      if FSettings.AlignVar and (FStack.GetTopType = rtVar) then
        FCurrentToken := AlignExpression(FTokenIdx, FSettings.AlignVarPos);

    rtVar, rtType:
      if not (FStack.GetTopType in [rtLeftBr, rtLeftHook]) then begin
        FWrapIndent := False;
        if FStack.nIndent < 1 then
          FStack.nIndent := 1;
        if (FStack.GetTopType in [rtVar, rtType]) then begin
          if (FCurrentRType = rtType) and IsRType(FPrevToken, rtEquals) then begin
            // in classes.pas I found
            // t = type AnsiString
            FStack.Pop
          end else if FStack.GetType(1) in [rtClass, rtClassDecl, rtRecord] then begin
            FStack.Pop;
            DecPrevLineIndent;
          end else
            FStack.Pop;
        end;
        if (FStack.GetTopType in [rtClass, rtClassDecl, rtRecord]) then begin
          FStack.Push(FCurrentRType, 1);
        end else begin
          if (FStack.GetTopType = rtProcDeclare) then begin
            // inline function
            FStack.Push(FCurrentRType, 1);
            if not IsRType(FPrevToken, rtEquals) then begin
              if FSettings.FeedAfterVar then
                AssertLineFeedAfter(FTokenIdx);
            end;
          end else begin
            if ((FPrevToken is TLineFeed) and (FStack.GetTopType = rtBegin)) then begin
              // inline var declaration
            end else if (FPrevToken.GetExpression(exp) and SameText(exp, 'for')) then begin
              // for with inline var declaration
            end else begin
              FStack.Push(FCurrentRType, 0);
              if not IsRType(FPrevToken, rtEquals) then begin
                DecPrevLineIndent;
                if FSettings.FeedAfterVar then
                  AssertLineFeedAfter(FTokenIdx);
              end;
            end;
          end;
        end;
      end;

    rtCase: begin
        if not (FStack.GetTopType in [rtRecord, rtLeftBr]) then
          FStack.Push(FCurrentRType, 0)
        else begin
          FWrapIndent := False;
          FStack.Push(rtRecCase, 1);
        end;
      end;

    rtDo: begin
        if FStack.GetTopType in [rtWhile, rtOn] then begin
          FLastPopResType := FStack.GetTopType;
          FStack.Push(FCurrentRType, 1);
          FWrapIndent := False;

          if FSettings.NoFeedBeforeThen and (FPrevToken = FPrevLine) then begin
            FTokens.Extract(FTokenIdx - 1).Free;
            Dec(FTokenIdx);
            CheckSlashComment;
          end;

          if FSettings.FeedAfterThen then begin
            if AssertLineFeedAfter(FTokenIdx) <> FPrevLine then begin
              if (FLastPopResType in [rtOn, rtWhile]) and FSettings.ExceptSingle then
                CheckShortLine;
            end;
          end;
        end;
      end;

    rtEnd: begin
        FWrapIndent := False;

        repeat
          FLastPopResType := FStack.Pop;
        until FStack.IsEmpty or (FLastPopResType in [rtClass, rtClassDecl, rtRecord, rtTry, rtCase, rtBegin, rtAsm (* , rtVisibility *)]);

        if FStack.IsEmpty then
          FStack.nIndent := 0
        else begin
          if (FLastPopResType = rtCase) and (FStack.GetTopType = rtClass) then
            FLastPopResType := FStack.Pop;
        end;

        if FSettings.FeedBeforeEnd and (FPrevToken <> nil)
          and (GetToken(FTokenIdx - 1).ReservedType <> rtLineFeed) then begin
          FPrevLine := AssertLineFeedAfter(FTokenIdx - 1);
          FPrevToken := FPrevLine;
        end;

        if (FPrevToken = FPrevLine) then
          SetPrevLineIndent(_NTmp);

        if NoBeginTryIndent(FCurrentRType) then
          FStack.nIndent := FStack.nIndent + 1;
      end;

    rtComment: begin
        if FSettings.IndentComments and (FStack.GetTopType <> rtLeftHook) then
          FWrapIndent := False;

        if FStack.IsEmpty and (FStack.nIndent > 0) then begin
          FStack.nIndent := 0;
          SetPrevLineIndent(_NTmp);
        end;

        AdjustSpacing(GetToken(FTokenIdx + 1), FCurrentToken, FTokenIdx + 1);
        if (FPrevLine <> nil) and (FPrevLine = FPrevToken) then begin
          if not FSettings.IndentComments
            or (FCurrentToken.WordType in [wtFullOutComment, wtHalfOutComment]) then
            FPrevLine.NoOfSpaces := FPrevLine.OldNoOfSpaces
          else begin
            if _PrevOldNspaces >= 0 then
              FPrevLine.NoOfSpaces := FPrevLine.NoOfSpaces +
                (FPrevLine.OldNoOfSpaces - _PrevOldNspaces)
            else
              _PrevOldNspaces := FPrevLine.OldNoOfSpaces;
          end;
        end else if FSettings.AlignComments and (FCurrentToken.WordType = wtFullComment) then begin
          if TryGetToken(FTokenIdx + 1, Next) and (Next.ReservedType = rtLineFeed) then
            FCurrentToken := AlignExpression(FTokenIdx, FSettings.AlignCommentPos);
        end;
      end;

    rtSemiColon: begin
        Assert(False, '.CheckIndent: rtSemiColon');
        if not (FStack.GetTopType in [rtLeftBr, rtLeftHook]) then begin
          Assert(False, '.CheckIndent: Not (LeftBr or LeftHook)');
          if FStack.GetTopType = rtUses then begin
            if (FSettings.NoIndentUsesComma) and (FPrevToken is TLineFeed) then
              SetPrevLineIndent(-1);
          end;

          while (FStack.GetTopType in
            [rtDo, rtWhile, rtProcDeclare, rtThen, rtProgram, rtUses, rtColon, rtClassDecl, rtIfElse]) do begin
            Assert(False, '.CheckIndent: in while');
            FStack.Pop;
          end;

          // todo: This only works, if 'absolute' or any of the function directives ('overload',
          // 'stdcall' etc). are on the same line as the function / procedure declaration:
          // Look for 'absolute' or a function directive before the next line feed
          FWrapIndent := False;
          RemoveMe := 0;
          repeat
            Assert(False, '.CheckIndent: in repeat');
            Inc(RemoveMe);
          until not TryGetToken(FTokenIdx + RemoveMe, Next) or (not (Next.ReservedType in [{ rtComment, }rtLineFeed]));

          if Next <> nil then begin
            Assert(False, '.CheckIndent: Next <> nil');
            if (Next.ReservedType = rtAbsolute)
              or ((FStack.GetTopType in [rtProcedure, rtProcDeclare, rtClass])
              and (Next.ReservedType in [rtFuncDirective, rtForward])
              and (FStack.ProcLevel = 0)) then
              FWrapIndent := True
            else if FSettings.FeedAfterSemiColon
              and not (Next.ReservedType in [rtForward, rtFuncDirective, rtDefault]) then
              AssertLineFeedAfter(FTokenIdx);
          end;
        end;
      end;

    rtCompIf: begin
        // push current stack to preserve indenting from before the ifdef
        FStackStack.Push(FStack.Clone);
      end;
    rtCompElse: begin
        if not FStackStack.IsEmpty then begin
          // Free current stack and take a copy of the one from before the corresponding ifdef.
          FStack.Free;
          FStack := FStackStack.Top.Clone;
        end;
      end;
    rtCompEndif: begin
        // pop and free the saved stack
        if not FStackStack.IsEmpty then
          FStackStack.Pop.Free;
      end;
  end; // case FCurrentRType

  Assert(False, '.CheckIndent: after case');
  AdjustSpacing(FCurrentToken, FPrevToken, FTokenIdx);

  if not (FCurrentRType in [rtLineFeed, rtComment]) then begin
    Assert(False, '.CheckIndent: rtLineFeed or rtComment');
    _PrevOldNspaces := -1;
  end;
  Assert(False, Format('.CheckIndent: PrevOldNSpaces=%d', [_PrevOldNspaces]));

  if wType = wtCompDirective then begin
    Assert(False, '.CheckIndent: compiler directive');
    FWrapIndent := False;

    if not FSettings.IndentCompDirectives and IsRType(FPrevToken, rtLineFeed) then begin
      Assert(False, '.CheckIndent: IndentCompDirectives');
      _NTmp := -FStack.nIndent;
      Assert(False, Format('NTmp=%d', [_NTmp]));
      SetPrevLineIndent(_NTmp);
    end;

    if FSettings.UpperCompDirectives then
      UppercaseCompilerDirective(FCurrentToken);
  end;

  FPrevToken := FCurrentToken;
  Assert(False, '.CheckIndent: PrevToken: ' + FPrevToken.GetForDebug);
end;

procedure TCodeFormatterFormatter.doExecute(_Tokens: TPascalTokenList);
var
  NTmp: Integer;
  PrevOldNspaces: Integer;
begin
  FTokens := _Tokens;

  if FSettings.ChangeIndent then begin
    FPrevLine := nil;
    FPrevPrevLine := nil;
    FPrevToken := nil;
    FWrapIndent := True;
    FIsInInterfacePart := False;
    NTmp := 0;
    PrevOldNspaces := -1;
    // the StackStack is used to preserve indenting over IFDEF/ELSE/ENDIF statements
    FStackStack := TCodeFormatterStack.Create;
    try
      FTokenIdx := 0;
      while TryGetToken(FTokenIdx, FCurrentToken) do begin
        if (FCurrentToken is TExpression) and (FCurrentToken.GetContent = '{ 1st paragraph.') then
          gblAssertTraceOn := True;
        if (FCurrentToken is TExpression) and (FCurrentToken.GetContent = 'implementation') then
          gblAssertTraceOn := False;
        Assert(False, '**** .doExecute: CurrentToken: ' + FCurrentToken.GetForDebug);
        Assert(False, '.doExecute: Stack.Depth: ' + IntToStr(FStack.Depth) + ' .TopType: ' + GetEnumname(TypeInfo(TReservedType), Ord(FStack.GetTopType)) + ' .TopIndent: ' + IntToStr(FStack.GetTopIndent));
        Assert(False, '.doExecute: WrapIndent: ' + Ifthen(FWrapIndent, 'True', 'False'));
        Assert(False, '.doExecute: before CheckIndent: NTmp: ' + IntToStr(NTmp) + ' PrevOldNspaces: ' + IntToStr(PrevOldNspaces));
        CheckIndent(NTmp, PrevOldNspaces);
        Assert(False, '.doExecute: after CheckIndent:  NTmp: ' + IntToStr(NTmp) + ' PrevOldNspaces: ' + IntToStr(PrevOldNspaces));
        Inc(FTokenIdx);
      end;
    finally
      FreeAndNil(FStackStack);
    end;
    gblAssertTraceOn := False;
  end;

  // remove empty lines from the end
  FTokenIdx := FTokens.Count - 1;
  while (FTokenIdx > 0) and TokenAtIs(FTokenIdx, rtLineFeed) do begin
    FTokens.Extract(FTokenIdx).Free;
    Dec(FTokenIdx);
  end;

  if FSettings.WrapLines or FHasAligned then
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
    FPrevLine := TLineFeed.Create(PrevPrevLine.OldNoOfSpaces, FSettings.SpacePerIndent);
    FPrevLine.NoOfSpaces := PrevPrevLine.NoOfSpaces;
    FPrevLine.Wrapped := True;
    GetToken(ATokenIdx).SetSpace([spBefore], False);
    FTokens.AtInsert(ATokenIdx, FPrevLine);
    HasInserted := True;
  end;

var
  TokenIdx, j, k: Integer;
  K2, LineLen: Integer;
  Token: TPascalToken;
  Expression: TAlignExpression;
begin
  LineLen := 0;
  FPrevLine := nil;
  j := 0;
  TokenIdx := 0;
  while TokenIdx < FTokens.Count do begin
    Token := GetToken(TokenIdx);
    // GetLength as a side effect, adjusts the alignment
    Token.GetLength(LineLen);

    if FSettings.WrapLines and (Token is TAlignExpression)
      and (LineLen > FSettings.WrapPosition) then begin
      Expression := Token as TAlignExpression;
      k := Expression.NoOfSpaces - LineLen - FSettings.WrapPosition;

      if k < 1 then
        Expression.NoOfSpaces := 1
      else
        Expression.NoOfSpaces := k;

      LineLen := FSettings.WrapPosition;
    end;

    if Token.ReservedType = rtLineFeed then begin
      FPrevLine := TLineFeed(Token);
      if (LineLen > FSettings.WrapPosition) then
        LineLen := 0;

      j := TokenIdx;
    end;

    if FSettings.WrapLines and (LineLen > FSettings.WrapPosition) and (TokenIdx > j + 3) then begin
      k := TokenIdx - 1;
      K2 := 0;
      HasInserted := False;

      while (k >= j) and not HasInserted do begin
        if (GetToken(k).ReservedType in [rtThen, rtDo])
          or ((GetToken(k).ReservedType = rtElse)
          and (GetToken(k + 1).ReservedType <> rtIf)) then begin
          InsertLinefeed(k + 1);
          TokenIdx := j;
        end;

        if (K2 = 0) and (GetToken(k).Space(spAfter)
          or GetToken(k + 1).Space(spBefore)) then
          K2 := k + 1;

        Dec(k);
      end;

      if not HasInserted and (K2 <> 0) and (K2 > j) then begin
        InsertLinefeed(K2);
        TokenIdx := j;
      end;

      LineLen := 0;
    end;
    Inc(TokenIdx);
  end;
end;

end.
