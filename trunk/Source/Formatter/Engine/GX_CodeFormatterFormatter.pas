// generates formatted Pascal code from a token collection
// Original Author:     Egbert van Nes (http://www.dow.wau.nl/aew/People/Egbert_van_Nes.html)
// Contributors:        Thomas Mueller (http://www.dummzeuch.de)
//                      Jens Borrisholt (Jens@borrisholt.dk) - Cleaning up the code, and making it aware of several language features

unit GX_CodeFormatterFormatter;

{$I GX_CondDefine.inc}

{$IFDEF GX_AV_IN_CODE_FORMATTER_FIX_ENABLED}
// This is a workaround for bug #314 Random operation of Formatter
// (https://sourceforge.net/p/gexperts/bugs/314/)
// The Delphi 11 compiler seems to have a bug that causes the code in the last while loop of
// of TCodeFormatterFormatter.doExecute to sometimes raise an access violation.
// On my computer I have only seen this happen once. On the computer of Ian (who created
// the bug report) it happens frequently, but not always.
// We have not been able to reliably reproduce the problem nor found the conditions which causes it.
// Turning optimization off solved the issue for him.
{$OPTIMIZATION OFF}
{$ENDIF}

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
    // Tracks compiler directive nesting level for icdIndependent mode
    FCompDirectiveNestLevel: Integer;
    // Tracks inline if expression (ternary operator) nesting depth
    FInlineIfDepth: Integer;
    FLastPopResType: TReservedType;
    // True between 'interface' and 'implementation'
    FIsInInterfacePart: Boolean;
    FWrapIndent: Boolean;
    // stores WrapIndent from before an opening bracket until the closing one
    FOldWrapIndent: Boolean;
    procedure UppercaseCompilerDirective(_Token: TPascalToken); {$IFDEF GX_SupportsInline} inline; {$ENDIF}
    function NoBeginTryIndent(_rType: TReservedType): Boolean; {$IFDEF GX_SupportsInline} inline; {$ENDIF}
    procedure IndentProcedureComment;
    procedure SetPrevLineIndent(_Value: Integer); {$IFDEF GX_SupportsInline} inline; {$ENDIF}
    procedure AddToPrevLineIndent(_Additional: Integer); {$IFDEF GX_SupportsInline} inline; {$ENDIF}
    procedure DecPrevLineIndent; {$IFDEF GX_SupportsInline} inline; {$ENDIF}

    ///<summary>
    /// replaces a TExpression with a TAlignExpression </summary>
    function AlignExpression(_Idx: Integer; _Pos: Integer): TPascalToken;
    procedure CheckWrapping;
    function IsRType(_Token: TPascalToken; _rType: TReservedType): Boolean; overload; {$IFDEF GX_SupportsInline} inline; {$ENDIF}
    function IsRType(_Token: TPascalToken; _rTypeSet: TReservedTypeSet): Boolean; overload; {$IFDEF GX_SupportsInline} inline; {$ENDIF}
    function IsWType(_Token: TPascalToken; _wType: TWordType): Boolean; {$IFDEF GX_SupportsInline} inline; {$ENDIF}
    ///<summary>
    /// Checks and corrects the number of blank lines before a procedure / function declaration </summary>
    procedure CheckBlankLinesAroundProc;
    ///<summary>
    /// Checks and corrects the number of blank lines before a uses clause </summary>
    procedure CheckBlankLinesBeforeUses;
    //procedure PutCommentBefore(const _Comment: TGXUnicodeString);
    procedure FormatAsm(_NTmp: Integer);
    procedure AdjustSpacing(_CurrentToken, _PrevToken: TPascalToken; _TokenIdx: Integer);

    ///<summary>
    /// @returns the token at index Idx or nil if out of bounds </summary>
    function GetToken(_Idx: Integer): TPascalToken; {$IFDEF GX_SupportsInline} inline; {$ENDIF}
    ///<summary>
    /// Get token at index Idx,
    /// @returns True and the Token if there is one
    ///          False and Token=nil if index is out of bounds </summary>
    function TryGetToken(_Idx: Integer; out _Token: TPascalToken): Boolean; {$IFDEF GX_SupportsInline} inline; {$ENDIF}
    ///<summary>
    /// Check whether the token at index Idx has the reserved type RType
    /// @param Idx is the index of the token to check
    /// @param RType is the queried reserverd type
    /// @returns True, if the token has the queried type, False otherwise
    ///          also False, if the index is out of bounds. </summary>
    function TokenAtIs(_Idx: Integer; _rType: TReservedType): Boolean; overload; {$IFDEF GX_SupportsInline} inline; {$ENDIF}
    function TokenAtIs(_Idx: Integer; _rTypes: TReservedTypeSet): Boolean; overload; {$IFDEF GX_SupportsInline} inline; {$ENDIF}

    function TryGetNextNoComment(_StartPos: Integer; _TreatEndIfAsComment: Boolean;
      out _Token: TPascalToken; out _Offset: Integer): Boolean; overload; {$IFDEF GX_SupportsInline} inline; {$ENDIF}
    function TryGetNextNoComment(_StartPos: Integer; _TreatEndIfAsComment: Boolean;
      out _Token: TPascalToken): Boolean; overload; {$IFDEF GX_SupportsInline} inline; {$ENDIF}
    function TryGetNextNoComment(_StartPos: Integer;
      out _Token: TPascalToken; out _Offset: Integer): Boolean; overload; {$IFDEF GX_SupportsInline} inline; {$ENDIF}
    function TryGetNextNoComment(_StartPos: Integer;
      out _Token: TPascalToken): Boolean; overload; {$IFDEF GX_SupportsInline} inline; {$ENDIF}

    function InsertBlankLines(_AtIndex, _NLines: Integer): TLineFeed;
    function AssertLineFeedAfter(_StartPos: Integer; _TreatEndIfAsComment: Boolean): TLineFeed; overload; {$IFDEF GX_SupportsInline} inline; {$ENDIF}
    function AssertLineFeedAfter(_StartPos: Integer): TLineFeed; overload; {$IFDEF GX_SupportsInline} inline; {$ENDIF}
    procedure CheckSlashComment;
    procedure ComplexIfElse(_NTmp: Integer);
    procedure CheckShortLine;

    ///<summary>
    /// This function does the actual formatting </summary>
    procedure doExecute(_Tokens: TPascalTokenList);
    procedure CheckIndentIf;
    procedure CheckIndentThen;
    procedure CheckIndentBeginTry(_NTmp: Integer);
    procedure CheckIndentColon;
    ///<summary>
    /// @returns True when FTokenIdx was changed and CheckIndent must exit </summary>
    function CheckIndentComma(): Boolean;
    procedure CheckIndentElse(_NTmp: Integer);
    procedure CheckIndentLeftBr;
    procedure CheckIndentProcedure(var _NTmp: Integer);
    ///<summary>
    /// @returns True when FTokenIdx was changed and CheckIndent must exit </summary>
    function CheckIndentSemicolon(): Boolean;
    function DetectGenericStart(_TokenIdx: Integer): Boolean;
    procedure CheckIndent(var _NTmp: Integer; var _PrevCommentLine: TLineFeed);
    procedure HandleCapitalization(_CurrentToken: TPascalToken);
    function TryGetPrevLineFeed(var _TokenIdx: Integer; out _LineFeed: TLineFeed): Boolean;
    function CheckForFunctTypeDeclare(out _CheckedToken: TPascalToken): Boolean;
    function AnonymousProcHadParams: Boolean;
    procedure CheckSpecialProcedure(var _NoBlankLine, _IsDelegate: Boolean);
    function FindBeginOrSemicolonAfterProc(out _Offset: Integer; out _Token: TPascalToken): Boolean;
    function InsertLinefeedBefore(ATokenIdx: Integer): Boolean;
    ///<summary>
    /// Detects if the current 'if' is used as an inline if expression (ternary operator) rather than a statement
    /// @returns True if this is an inline if expression </summary>
    function IsInlineIfExpression: Boolean;
    ///<summary>
    /// Calculates the column position of a token (0-based)
    /// @param TokenIdx is the index of the token
    /// @returns the column position where the token starts </summary>
    function GetTokenColumn(_TokenIdx: Integer): Integer;
    ///<summary>
    /// Finds the corresponding 'if' token for the current 'then' or 'else' in an inline if expression (ternary operator)
    /// @returns the token index of the matching 'if', or -1 if not found </summary>
    function FindMatchingInlineIf: Integer;
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
  u_dzAssertTrace,
  GX_StringList;

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
  FCompDirectiveNestLevel := 0;
  FInlineIfDepth := 0;
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
  Result := TAlignExpression.Create(OldExpr, _Pos, OldExpr.OrigLineNo);
  FTokens.AtInsert(_Idx, Result);
  OldExpr.Free;
end;

// inlined method must be implemented before it is used
function TCodeFormatterFormatter.IsWType(_Token: TPascalToken; _wType: TWordType): Boolean;
begin
  Result := Assigned(_Token) and (_Token.WordType = _wType);
end;

// inlined method must be implemented before it is used
function TCodeFormatterFormatter.TryGetToken(_Idx: Integer; out _Token: TPascalToken): Boolean;
begin
  Result := (_Idx >= 0) and (_Idx < FTokens.Count);
  if Result then
    _Token := FTokens[_Idx]
  else
    _Token := nil;
end;

// inlined method must be implemented before it is used
function TCodeFormatterFormatter.TryGetNextNoComment(_StartPos: Integer; _TreatEndIfAsComment: Boolean;
  out _Token: TPascalToken; out _Offset: Integer): Boolean;
begin
  _Offset := 0;

  repeat
    Inc(_Offset);
    Result := TryGetToken(_StartPos + _Offset, _Token);
  until not Result
    or ((_Token.ReservedType <> rtComment) and
    (not _TreatEndIfAsComment or (_Token.GetReservedType <> rtCompEndif)));
end;

// inlined method must be implemented before it is used
function TCodeFormatterFormatter.TryGetNextNoComment(_StartPos: Integer;
  out _Token: TPascalToken; out _Offset: Integer): Boolean;
begin
  Result := TryGetNextNoComment(_StartPos, False, _Token, _Offset)
end;

// inlined method must be implemented before it is used
function TCodeFormatterFormatter.TryGetNextNoComment(_StartPos: Integer; out _Token: TPascalToken): Boolean;
var
  Offset: Integer;
begin
  Result := TryGetNextNoComment(_StartPos, False, _Token, Offset);
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

  if not FCurrentToken.TryGetExpression(exp) or (exp <> '<') then
    Exit; //==>

  if not IsWType(FPrevToken, wtWord) then
    Exit; //==>

  Idx := 0;
  rType := FStack.GetType(Idx);
  while rType = rtGenericStart do begin
    Inc(Idx);
    rType := FStack.GetType(Idx);
  end;
  if rType in [rtClass, rtType, rtVar, rtConst, rtProcedure, rtProcDeclare] then begin
    Result := True;
    Exit; //==>
  end;

  // These were the easy cases.
  // Now we must detect whether a '<' is followed by a '>' with reasonable intermediate tokens.

  if not TryGetNextNoComment(_TokenIdx, Next, Offset) then
    Exit; //==>

  // the next token must be an identifier (= rtNothing or rtReserved in the special case of 'string') (correct?)
  if not (Next.ReservedType in [rtNothing, rtReserved]) or (Next.WordType <> wtWord) then
    Exit; //==>

  Idx := _TokenIdx + Offset;
  while TryGetNextNoComment(Idx, Next, Offset) do begin
    case Next.ReservedType of
      rtLogOper: begin
          if Next.TryGetExpression(exp) and (exp = '>') then begin
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
      rtComma,
      rtDot: begin
        // OK, dots appear in qualified type names like System.TJSONObject
        end;
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
      // todo: TCodeFormatterSettings.HandleCapitalization shouldn't be a method of
      //       the Settings class
      FSettings.HandleCapitalization(_CurrentToken);
    end;
  end;
end;

// inlined method must be implemented before it is used
function TCodeFormatterFormatter.IsRType(_Token: TPascalToken; _rType: TReservedType): Boolean;
begin
  Result := Assigned(_Token) and (_Token.ReservedType = _rType);
end;

// inlined method must be implemented before it is used
function TCodeFormatterFormatter.IsRType(_Token: TPascalToken;
  _rTypeSet: TReservedTypeSet): Boolean;
begin
  Result := Assigned(_Token) and (_Token.ReservedType in _rTypeSet);
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
                 it also unnecessarily sets each token's case three times
                 Also note that the parser already sets the capitalization using the
                 ExpressionCase property. }
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

    rtIf: begin
        Assert(False, '.AdjustSpacing');
        _CurrentToken.SetSpace([spAfter], True);
        // Add extra space after 'if' when DoubleIndentIfCondition is enabled
        // This aligns the first condition with continuation lines
        if FSettings.DoubleIndentIfCondition then
          _CurrentToken.SetExpression('if ');
      end;

    rtUntil, rtWhile, rtCase, rtRecord: begin
        Assert(False, '.AdjustSpacing');
        _CurrentToken.SetSpace([spAfter], True);
      end;

    rtLogOper: begin
        Assert(False, '.AdjustSpacing');
        if not _CurrentToken.TryGetExpression(exp) then
          raise EFormatException.Create('Programmer error: TryGetExpression for logical operator returned False');

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
            // todo: This does nothing! Is it supposed to remove any spaces?
            //       In that case it should be SetSpace([spBefore, spAfter], False)
            _CurrentToken.SetSpace([], True);
            _CurrentToken.SetReservedType(rtGenericEnd);
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
        // todo: This would prevent a space being inserted between ';' and '{$ENDIF}'
        // In theory it does not remove an existing space, but that does not
        // matter because the parser does not preserve these spaces.
        // So either we should have a special setting for spaces before/after
        // compiler directives or we should leave it as is. Unfortunately
        // leaving it as is woould mean that the parser should presever the
        // spaces which it does not, so we would have to change the parser.
        // for now I'll simply disable this code again.
//        if _TokenIdx < FTokens.Count - 1 then
//          if FTokens[_TokenIdx + 1].ReservedType <> rtCompEndif then
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

    rtPropertyAccess: begin
        if _PrevToken.ReservedType = rtGenericEnd then
          _CurrentToken.SetSpace([spBefore], True);
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
        rtMinus, rtLogOper, rtEquals, rtAssignOper, rtLeftBr, rtColon,
        rtLeftHook, rtComma, rtDefault, rtThen, rtElse]) then begin
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

function TCodeFormatterFormatter.InsertBlankLines(_AtIndex, _NLines: Integer): TLineFeed;
var
  LineIdx: Integer;
  NextToken: TPascalToken;
begin
  Result := FPrevLine;

  for LineIdx := 0 to _NLines - 1 do begin //FI:W528
    Result := TLineFeed.Create(0, FSettings.SpacePerIndent, -1);
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

function TCodeFormatterFormatter.AssertLineFeedAfter(_StartPos: Integer; _TreatEndIfAsComment: Boolean): TLineFeed;
var
  Next: TPascalToken;
  Offset: Integer;
begin
  if TryGetNextNoComment(_StartPos, _TreatEndIfAsComment, Next, Offset)
    and (Next.ReservedType <> rtLineFeed) then
    Result := InsertBlankLines(_StartPos + Offset, 1)
  else
    Result := FPrevLine;
end;

function TCodeFormatterFormatter.AssertLineFeedAfter(_StartPos: Integer): TLineFeed;
begin
  Result := AssertLineFeedAfter(_StartPos, False);
end;

procedure TCodeFormatterFormatter.DecPrevLineIndent;
begin
  if FPrevLine <> nil then
    FPrevLine.IncIndent(-1);
end;

function TCodeFormatterFormatter.TryGetNextNoComment(_StartPos: Integer; _TreatEndIfAsComment: Boolean;
  out _Token: TPascalToken): Boolean;
var
  Offset: Integer;
begin
  Result := TryGetNextNoComment(_StartPos, _TreatEndIfAsComment, _Token, Offset);
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

  // done: This can be implemented more efficiently because we know that
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

  Len := Length(CommentLFs);
  if Len = 0 then begin
    // Fix for bug #318: Formatter fails with comment after function declaration in implementation
    // for the case:
    {
    procedure bla; // <== this is the comment before the procedure blub in the next line
    procedure blub;
    }
    // There is no other comment before that, so the array is empty.
    Exit; //==>
  end;

  // We assume that the previous comments have already been arranged correctly
  // to match the first comment. So we determine the adjustment for that first
  // comment and apply it to all comment lines.

  DesiredNoOfSpaces := ThisLineStart.NoOfSpaces;
  LineFeed := CommentLFs[Len - 1];
  Diff := LineFeed.NoOfSpaces - DesiredNoOfSpaces;
  LineFeed.NoOfSpaces := DesiredNoOfSpaces;
  for i := 0 to Len - 2 do begin
    LineFeed := CommentLFs[i];
    LineFeed.NoOfSpaces := LineFeed.NoOfSpaces - Diff;
  end;
end;

procedure TCodeFormatterFormatter.AddToPrevLineIndent(_Additional: Integer);
begin
  if FPrevLine <> nil then
    FPrevLine.SetIndent(FStack.nIndent + _Additional + FStack.ProcLevel);
end;

procedure TCodeFormatterFormatter.SetPrevLineIndent(_Value: Integer);
begin
  if FPrevLine <> nil then
    FPrevLine.SetIndent(_Value);
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
  Len: Integer;
begin
  if not _Token.TryGetExpression(s) then
    Exit; //==>

  Len := Length(s);
  Idx := 2;
  while (Idx < Len) and (s[Idx] <> Space) and (s[Idx] <> Tab) do begin
    s[Idx] := UpCase(s[Idx]);
    Inc(Idx);
  end;
  if _Token.ReservedType = rtCompMessage then begin
    // {$MESSAGE WARN 'xxxx'}
    // -> We also want to upper case the message type
    while (Idx < Len) and (s[Idx] = Space) or (s[Idx] = Tab) do
      Inc(Idx);
    while (Idx < Len) and (s[Idx] <> Space) and (s[Idx] <> Tab) do begin
      s[Idx] := UpCase(s[Idx]);
      Inc(Idx);
    end;
  end;

  _Token.SetExpression(s);
end;

procedure TCodeFormatterFormatter.CheckBlankLinesAroundProc;
var
  k: Integer;
  Prev2: TPascalToken;
begin
  if FPrevToken = nil then
    Exit; //==>

  if IsRType(FPrevToken, rtClass) then begin
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
    // todo -cfixme : This doesn't work. At least not for ...
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

    if (Prev2.ReservedType in [rtComment, rtCompIf, rtCompElse]) and TokenAtIs(FTokenIdx - k - 1, rtLineFeed) then begin
      // There is a comment before the procedure. Check for more comments before that.
      Inc(k, 2);
      while TryGetToken(FTokenIdx - k, Prev2) and (Prev2.ReservedType in [rtComment, rtCompIf, rtCompElse])
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

procedure TCodeFormatterFormatter.CheckBlankLinesBeforeUses;
var
  k: Integer;
  Prev2: TPascalToken;
begin
  if FPrevToken = nil then
    Exit; //==>

  k := 1;
  Prev2 := FPrevToken;

  if not IsRType(Prev2, rtLineFeed) then begin
    // no line feed at all -> add two for an empty line
    FPrevLine := InsertBlankLines(FTokenIdx - k, 2);
    FPrevToken := FPrevLine;
  end else begin
    // That's the line feed at the begin of this line.
    // Now check for another one before that
    Inc(k);
    if not TryGetToken(FTokenIdx - k, Prev2) or (Prev2.ReservedType = rtLineFeed) then begin
      // There is no previous token or there is already a second line feed, we are done.
      Exit; //==>
    end;

    if (Prev2.ReservedType in [rtComment, rtCompIf, rtCompElse]) and TokenAtIs(FTokenIdx - k - 1, rtLineFeed) then begin
      // There is a comment before the uses clause. Check for more comments before that.
      Inc(k, 2);
      while TryGetToken(FTokenIdx - k, Prev2) and (Prev2.ReservedType in [rtComment, rtCompIf, rtCompElse])
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
var
  AsmTokenIdx: Integer;
  IsAsmAfterLabel: Boolean;
begin
  // remove var / type stuff
  while FStack.GetTopType in [rtVar, rtConst, rtType] do
    FStack.Pop;

  // no additional indentation for
  // procedure xxx;
  // asm
  if FStack.GetTopType = rtProcedure then begin
    FStack.Pop;
    DecPrevLineIndent;
  end;

  FStack.Push(FCurrentRType, 0);

  AsmTokenIdx := FTokenIdx;

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

  if AsmTokenIdx > 2 then begin
    IsAsmAfterLabel := (FTokens[AsmTokenIdx - 2].ReservedType = rtlabel);
  end else
    IsAsmAfterLabel := False;

  if not IsAsmAfterLabel and (FTokenIdx < FTokens.Count) then
    AddToPrevLineIndent(_NTmp);

  Dec(FTokenIdx);
end;

// todo: Since this can fail, why not simply keep the line feed that caused this problem?
//       The user has used a // comment, so he should be the one to remove, move or replace it.
procedure TCodeFormatterFormatter.CheckSlashComment;

  ///<summary>
  /// changes " // <comment>" to "{//<comment>}"
  /// used for preventing a
  ///    begin
  /// moved from the next line to become commented out
  /// @returns True if token is a comment, False otherwise </summary>
  function ChangeSingleLineCommentToBeginEndComment(_Token: TPascalToken): Boolean;
  var
    s: TGXUnicodeString;
  begin
    Result := (_Token.ReservedType = rtComment);
    if not Result then begin
      // no comment
      Exit;
    end;

    if not _Token.TryGetExpression(s) then begin
      // no expression
      // todo: This should not be possible -> raise an exception?
      //       On the other hand: If it is not a comment we do not need to change it.
      Exit;
    end;

    if Copy(s, 1, 2) <> '//' then begin
      // not a double slash comment
      Exit;
    end;

    if Pos('}', s) <> 0 then begin
      // The comment contains '}' so we cannot convert it to a curly braces comment
      // Therefore we must use '(*'.
      if Pos('*)', s) <> 0 then begin
        // But it also contains '*)' so we cannot use that either
        raise Exception.CreateFmt('Cannot convert %s to one of the other comment types because the comment contains "}" and "*)"', [s]);
      end;
      s := '(*' + s + '*)';
    end else
      s := '{' + s + '}';

    _Token.SetExpression(s);
  end;

var
  Token: TPascalToken;
  PrevPasWord: TPascalToken;
  Expression: TGXUnicodeString;
  PrevExpression: TGXUnicodeString;
  i: Integer;
begin
  if TryGetToken(FTokenIdx - 1, FPrevToken) and (FPrevToken.ReservedType = rtComment)
    and FPrevToken.TryGetExpression(PrevExpression) and (Copy(PrevExpression, 1, 2) = '//') then begin
    // fix for situation with a // comment on prev line: begin becomes part of the comment
    if ChangeSingleLineCommentToBeginEndComment(FPrevToken) then begin
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
        and PrevPasWord.TryGetExpression(Expression)
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

  AddToPrevLineIndent(_NTmp);
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

procedure TCodeFormatterFormatter.CheckIndentIf;
var
  Idx: Integer;
  Token: TPascalToken;
  IsElseIf: Boolean;
begin
  Assert(False, '.CheckIndentIf');

  // Check if this is an inline if expression
  if IsInlineIfExpression then begin
    // Check if this is an 'else if' continuation (previous token is else)
    // In that case, we already didn't decrement at the else, so don't increment here
    IsElseIf := False;
    if FInlineIfDepth > 0 then begin
      Idx := FTokenIdx - 1;
      while Idx >= 0 do begin
        Token := GetToken(Idx);
        if (Token.ReservedType <> rtComment) and (Token.ReservedType <> rtLineFeed) then begin
          if Token.ReservedType = rtElse then
            IsElseIf := True;
          Break;
        end;
        Dec(Idx);
      end;
    end;

    if not IsElseIf then
      Inc(FInlineIfDepth);
    // For inline if, we just push rtIf on the stack and don't do any line feed manipulation
    FStack.Push(rtIf, 0);
    Exit;
  end;

  // Statement if processing (existing logic)
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

procedure TCodeFormatterFormatter.CheckIndentThen;
var
  IfTokenIdx: Integer;
  IfColumn: Integer;
  PrevIdx: Integer;
  PrevToken: TPascalToken;
  Idx: Integer;
  Token: TPascalToken;
begin
  Assert(False, '.CheckIndentThen');
  if FStack.GetTopType in [rtIf, rtIfElse] then begin
    FWrapIndent := False;
    FLastPopResType := FStack.Pop;

    // For inline if expressions, handle vertical formatting
    if FInlineIfDepth > 0 then begin
      // Check if 'then' is on a new line (vertical formatting)
      // Look backwards to see if there's a line feed (skipping comments)
      PrevIdx := FTokenIdx - 1;
      while (PrevIdx >= 0) do begin
        PrevToken := GetToken(PrevIdx);
        if PrevToken.ReservedType = rtLineFeed then begin
          // Found a line feed - this is vertical formatting
          // For 'then', find the corresponding 'if' by searching backwards for the first 'if'
          IfTokenIdx := -1;
          Idx := FTokenIdx - 1;
          while Idx >= 0 do begin
            Token := GetToken(Idx);
            if (Token.ReservedType <> rtComment) and (Token.ReservedType <> rtLineFeed) then begin
              if Token.ReservedType = rtIf then begin
                IfTokenIdx := Idx;
                Break;
              end;
            end;
            Dec(Idx);
          end;

          if IfTokenIdx >= 0 then begin
            // Calculate column position of 'if' and indent 'then' relative to it
            IfColumn := GetTokenColumn(IfTokenIdx);
            TLineFeed(PrevToken).NoOfSpaces := IfColumn + FSettings.SpacePerIndent;
            if FPrevLine = PrevToken then
              FPrevLine.NoOfSpaces := IfColumn + FSettings.SpacePerIndent;
          end;
          Break;
        end else if PrevToken.ReservedType <> rtComment then begin
          // Found a non-comment, non-linefeed token - not vertical formatting
          Break;
        end;
        Dec(PrevIdx);
      end;
      FStack.Push(rtThen, 0);  // Push with no indent change
      Exit;
    end;

    // Statement if processing (existing logic)
    if FSettings.NoFeedBeforeThen and (FPrevToken = FPrevLine)
      and (GetToken(FTokenIdx - 1).ReservedType <> rtComment) then begin
      FTokens.Extract(FTokenIdx - 1).Free;
      Dec(FTokenIdx);
      CheckSlashComment;
    end;
    if FSettings.FeedAfterThen then begin
      if AssertLineFeedAfter(FTokenIdx) <> FPrevLine then begin
        if (FLastPopResType = rtIf) and FSettings.FeedAfterThenExceptSingle then
          CheckShortLine;
      end;
    end;
    FStack.Push(rtThen, 1);
  end;
end;

procedure TCodeFormatterFormatter.CheckIndentBeginTry(_NTmp: Integer);
var
  FeedRound: TFeedBegin;
begin
  while FStack.GetTopType in [rtVar, rtConst, rtType] do
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
        // todo: could this use TryGetToken instead of checking for FPrevToken <> nil ?
        if (FPrevToken <> nil) and (GetToken(FTokenIdx - 1).ReservedType <> rtLineFeed) then begin
          FPrevLine := AssertLineFeedAfter(FTokenIdx - 1);
          FPrevToken := FPrevLine;
        end;

        AssertLineFeedAfter(FTokenIdx);
      end;
  end;

  FStack.Push(FCurrentRType, 1);
  if FPrevToken = FPrevLine then begin
    AddToPrevLineIndent(_NTmp);
    DecPrevLineIndent;
  end;

  FWrapIndent := False;
end;

procedure TCodeFormatterFormatter.CheckIndentColon;
var
  PrevPrevToken: TPascalToken;
  Token: TPascalToken;
begin
  Assert(False, '.CheckIndentColon');
  case FStack.GetTopType of
    rtOf: begin
        FStack.Push(FCurrentRType, 1);
        if FSettings.FeedAfterThen then begin
          if TryGetNextNoComment(FTokenIdx, Token)
            and (Token.ReservedType = rtBegin)
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
      if FSettings.ShouldAlignVar then
        FCurrentToken := AlignExpression(FTokenIdx, FSettings.AlignVarPos);

    rtConst:
      if FSettings.ShouldAlignConst then
        FCurrentToken := AlignExpression(FTokenIdx, FSettings.AlignVarPos);

    rtProcedure, rtProcDeclare:
      ; // do nothing

    rtRecCase: begin
        // Variant record case label (e.g., "True:" or "False:" in variant part)
        // Only push indentation if there's a linebreak after the colon
        // (i.e., variant content is on the next line, not the same line)
        if TryGetNextNoComment(FTokenIdx, Token) and (Token.ReservedType = rtLineFeed) then
          FStack.Push(FCurrentRType, 1);
        FWrapIndent := False;
      end;

    rtClass, rtRecord, rtGenericStart, rtLeftBr, rtOn:
      FWrapIndent := False;

  else
    // label
    // maybe this could be reduced to rtBegin ?
    if (FPrevToken.WordType in [wtWord, wtNumber]) and (FPrevToken.ReservedType = rtNothing) then begin
      if (FTokenIdx > 2) then begin
        PrevPrevToken := FTokens[FTokenIdx - 2];
        case PrevPrevToken.ReservedType of
          rtLineFeed: begin
              FPrevToken.ReservedType := rtlabel;
              if FSettings.UnindentLabels then
                FPrevLine.NoOfSpaces := 0;
            end;
          rtSemiColon: begin
              FPrevToken.ReservedType := rtlabel;
              if FSettings.UnindentLabels then begin
                if InsertLinefeedBefore(FTokenIdx - 1) then begin
                  Inc(FTokenIdx);
                  FPrevLine.NoOfSpaces := 0;
                  FPrevLine.OldNoOfSpaces := 0;
                  FPrevLine.Wrapped := False;
                end
              end;
            end;
        end;
      end;
    end;

    FWrapIndent := False;
  end;
end;

function TCodeFormatterFormatter.CheckIndentComma: Boolean;
var
  IdxChanged: Boolean;
  Next: TPascalToken;
  Offset: Integer;
  LFeed: TLineFeed;
begin
  Assert(False, '.CheckIndentComma');

  Result := False;
  if FStack.GetTopType = rtUses then begin
    if (FSettings.NoIndentUsesComma) and (FPrevToken is TLineFeed) then
      AddToPrevLineIndent(-1)
    else if FSettings.FeedEachUnit then begin
      if FSettings.FeedEachUnitBeforeComma then begin
        // linefeed before comma
        if (not (FPrevToken is TLineFeed)) then begin
          IdxChanged := False;
          Next := GetToken(FTokenIdx + 1);
          if Next.ReservedType = rtComment then begin
            // The next Token is a comment, we don't want this comment to
            // be the first after the break which would comment out the next
            // unit, so we need to move it.

            // Move the Comma behind the comment
            FTokens.AtInsert(FTokenIdx + 1, FTokens.Extract(FTokenIdx));
            FTokenIdx := FTokenIdx + 1;
            IdxChanged := True;
          end;

          LFeed := AssertLineFeedAfter(FTokenIdx - 1);
          if (FSettings.NoIndentUsesComma) then
            LFeed.SetIndent(0);

          // If we have a break after the comma, remove that
          while TryGetNextNoComment(FTokenIdx, Next, Offset)
            and (Next.ReservedType = rtLineFeed) do begin
            FTokens.Extract(FTokenIdx + Offset).Free;
          end;

          // We changed the code, we need to process the comment again
          if IdxChanged then begin
            FTokenIdx := FTokenIdx - 3;
            Result := True;
            Exit; //==>
          end;
        end;
      end else begin
        // not FeedEachUnitBeforeComma
        if TryGetNextNoComment(FTokenIdx, True, Next) and (Next.ReservedType <> rtLineFeed) then
          AssertLineFeedAfter(FTokenIdx, True);
      end;
    end;
  end;
end;

procedure TCodeFormatterFormatter.CheckIndentElse(_NTmp: Integer);
var
  Next: TPascalToken;
  IfTokenIdx: Integer;
  IfColumn: Integer;
  PrevIdx: Integer;
  PrevToken: TPascalToken;
begin
  Assert(False, '.CheckIndentElse');

  FLastPopResType := rtNothing;

  while not FStack.IsEmpty and not (FStack.GetTopType in [rtThen, rtOf, rtTry]) do
    FLastPopResType := FStack.Pop;

  // For inline if expressions, handle vertical formatting
  if FInlineIfDepth > 0 then begin
    // Check if 'else' is on a new line (vertical formatting)
    // The previous token should be a line feed (or comments then line feed)
    PrevIdx := FTokenIdx - 1;
    while (PrevIdx >= 0) do begin
      PrevToken := GetToken(PrevIdx);
      if PrevToken.ReservedType = rtLineFeed then begin
        // Found a line feed - this is vertical formatting
        // Find the corresponding 'if' token
        IfTokenIdx := FindMatchingInlineIf;
        if IfTokenIdx >= 0 then begin
          // Calculate column position of 'if' and indent 'else' relative to it
          IfColumn := GetTokenColumn(IfTokenIdx);
          // Set the line feed's indentation
          TLineFeed(PrevToken).NoOfSpaces := IfColumn + FSettings.SpacePerIndent;
          if FPrevLine = PrevToken then
            FPrevLine.NoOfSpaces := IfColumn + FSettings.SpacePerIndent;
        end;
        Break;
      end else if PrevToken.ReservedType <> rtComment then begin
        // Found a non-comment, non-linefeed token - not vertical formatting
        Break;
      end;
      Dec(PrevIdx);
    end;
    // Push rtElse or rtIfElse with no indent change
    if FStack.GetTopType = rtThen then
      FStack.Push(rtIfElse, 0)
    else
      FStack.Push(rtElse, 0);
    // Decrement depth as we're finishing this inline if
    // EXCEPTION: If next token is 'if', this is a nested inline if (else if ...)
    // Don't decrement - the nested 'if' will see FInlineIfDepth > 0 and handle correctly
    if not (TryGetNextNoComment(FTokenIdx, Next) and (Next.ReservedType = rtIf)) then
      Dec(FInlineIfDepth);
    FWrapIndent := False;
    Exit;
  end;

  // Statement if/else processing (existing logic)
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
    AddToPrevLineIndent(_NTmp);

  if FSettings.FeedBeforeElse then begin
    if not TokenAtIs(FTokenIdx - 1, rtLineFeed) then begin
      FPrevLine := AssertLineFeedAfter(FTokenIdx - 1);
      FPrevToken := FPrevLine;
    end;
  end;

  if FSettings.IndentTryElse and (FStack.GetTopType = rtTry) then begin
    FStack.nIndent := FStack.nIndent + 1;
    AddToPrevLineIndent(_NTmp);
  end else if FSettings.IndentCaseElse and (FStack.GetTopType = rtOf) then begin
    FStack.nIndent := FStack.nIndent + 1;
    AddToPrevLineIndent(_NTmp);
  end;

  if FStack.GetTopType = rtThen then
    FStack.Push(rtIfElse, 1)
  else
    FStack.Push(rtElse, 1);

  FWrapIndent := False;
end;

procedure TCodeFormatterFormatter.CheckIndentLeftBr();
var
  Next: TPascalToken;
  Offset: Integer;
begin
  Assert(False, '.CheckIndentLeftBr');
  if (FStack.GetTopType = rtLeftBr) then begin
    Assert(False, '.CheckIndentLeftBr: Top is LeftBr');
    // Nested parentheses for function calls should add indentation
    // but not for record/array constants (where prev token is not a word)
    if (FPrevToken.WordType = wtWord) then
      FStack.Push(FCurrentRType, 1)
    else
      FStack.Push(FCurrentRType, 0)
  end else begin
    Assert(False, '.CheckIndentLeftBr: Top is not Leftbr');
    FOldWrapIndent := FWrapIndent;
    if (FStack.ProcLevel <= 0) or (FStack.GetTopType <> rtProcedure) then begin
      Assert(False, '.CheckIndentLeftBr');
      // not very clean
      FStack.Push(FCurrentRType, 1);
    end else begin
      Assert(False, '.CheckIndentLeftBr');
      Offset := 1;
      while (FTokenIdx > Offset) and (TryGetToken(FTokenIdx - Offset, Next)
        and (Next.ReservedType in [rtDot, rtNothing])) do begin
        Assert(False, '.CheckIndentLeftBr');
        Inc(Offset);
      end;
      if IsRType(Next, rtProcedure) then begin
        Assert(False, '.CheckIndentLeftBr');
        FStack.Push(FCurrentRType, 0);
      end else begin
        Assert(False, '.CheckIndentLeftBr');
        FStack.Push(FCurrentRType, 1);
      end;
    end;
    Assert(False, '.CheckIndentLeftBr');
    FWrapIndent := False;
  end;
end;

function TCodeFormatterFormatter.CheckIndentSemicolon(): Boolean;
var
  IdxChanged: Boolean;
  Next: TPascalToken;
  Offset: Integer;
  LFeed: TLineFeed;
  AllowPop: Boolean;
begin
  Assert(False, '.CheckIndentSemicolon');

  Result := False;

  if FStack.GetTopType = rtExternal then begin
    FStack.Pop;
  end;

  if not (FStack.GetTopType in [rtLeftBr, rtLeftHook, rtGenericStart]) then begin
    Assert(False, '.CheckIndent: Not (LeftBr or LeftHook)');
    if FStack.GetTopType = rtUses then begin
      if (FPrevToken is TLineFeed) then begin
        if (FSettings.NoIndentUsesComma) then
          AddToPrevLineIndent(-1);
      end else if (FSettings.FeedEachUnit and FSettings.FeedEachUnitBeforeComma) then begin
              // if we break before the comma, also break before the semicolon

        IdxChanged := False;
        Next := GetToken(FTokenIdx + 1);
        if Next.ReservedType = rtComment then begin
                // The next Token is a comment, we don't want this comment to
                // be the first after the break which would comment out the next
                // unit, so we need to move it.

                // Move the Comma behind the comment
          FTokens.AtInsert(FTokenIdx + 1, FTokens.Extract(FTokenIdx));
          FTokenIdx := FTokenIdx + 1;
          IdxChanged := True;
        end;

        LFeed := AssertLineFeedAfter(FTokenIdx - 1);
        if (FSettings.NoIndentUsesComma) then
          LFeed.SetIndent(0);

        // We changed the code, we need to process the comment again
        if IdxChanged then begin
          FTokenIdx := FTokenIdx - 3;
          Result := True;
          Exit; //==>
        end;
      end;
    end;

    AllowPop := True;
    if FStack.GetTopType = rtProcDeclare then begin
      // this checks for function declarations with function directives in the next line like this:
      // procedure bla;
      //   cdecl;
      // It does not yet solve the problem with multi line function directives like this:
      // procedure bla;
      //   cdecl;
      //   overload;
      Offset := 1;
      while TryGetToken(FTokenIdx + Offset, Next)
        and IsRType(Next, [rtSemiColon, rtLineFeed, rtComment, rtCompIf, rtCompElse, rtCompEndif,
          rtFuncDirective, rtForward, rtExternal]) do begin
        if IsRType(Next, [rtFuncDirective, rtForward, rtExternal]) then begin
          AllowPop := False;
          Break; //==v
        end;
        Inc(Offset);
      end
    end;

    if AllowPop then begin
      while (FStack.GetTopType in
        [rtDo, rtWhile, rtProcDeclare, rtThen, rtProgram, rtUses, rtColon, rtClassDecl, rtIfElse]) do begin
        Assert(False, '.CheckIndent: in while');
        FStack.Pop;
      end;
    end;

    if (FPrevToken.ReservedType = rtForward) and (FStack.GetTopType = rtProcedure) then begin
      FStack.Pop;
    end;

          // Look for 'absolute' or a function directive before the next line feed
          // todo: This only works, if 'absolute' or any of the function directives ('overload',
          // 'stdcall' etc). are on the same line as the function / procedure declaration:
    FWrapIndent := False;
    Offset := 0;
    repeat
      Assert(False, '.CheckIndent: in repeat');
      Inc(Offset);
    until not TryGetToken(FTokenIdx + Offset, Next) or (not (Next.ReservedType in [{ rtComment, }rtLineFeed]));

    if Next <> nil then begin
      Assert(False, '.CheckIndent: Next <> nil');
      if (Next.ReservedType = rtAbsolute)
        or ((FStack.GetTopType in [rtProcedure, rtProcDeclare, rtClass])
        and (Next.ReservedType in [rtFuncDirective, rtForward, rtExternal])
        and (FStack.ProcLevel = 0)) then
        FWrapIndent := True
      else if FSettings.FeedAfterSemiColon then begin
        if not (Next.ReservedType in [rtForward, rtExternal, rtFuncDirective, rtDefault]) then begin
          if Next.WordType <> wtCompDirective then begin
            AssertLineFeedAfter(FTokenIdx);
          end;
        end;
      end;
    end;
  end;
end;

function TCodeFormatterFormatter.CheckForFunctTypeDeclare(out _CheckedToken: TPascalToken): Boolean;
var
  TempWordIdx: Integer;
begin
  Assert(IsRType(FCurrentToken, rtProcedure));
  // search for the previous token that is not a comment or LineFeed
  TempWordIdx := FTokenIdx - 1;
  while TryGetToken(TempWordIdx, _CheckedToken) and IsRType(_CheckedToken, [rtComment, rtLineFeed]) do begin
    Assert(False, '.CheckIndent: in while');
    Dec(TempWordIdx);
  end;
  Result := IsRType(_CheckedToken, [rtEquals, rtColon, rtComma, rtLeftBr, rtAssignOper]);
end;

function TCodeFormatterFormatter.AnonymousProcHadParams: Boolean;
var
  TempIdx: Integer;
  Token: TPascalToken;
begin
  // Check if the anonymous procedure (whose declaration we're inside) had parameters
  // by scanning backwards from the current position to find ')' before any line feeds
  // This is used to determine if we should add a blank line before nested content
  Result := False;
  TempIdx := FTokenIdx - 1;
  // Skip line feeds and comments
  while TryGetToken(TempIdx, Token) and (Token.ReservedType in [rtLineFeed, rtComment]) do
    Dec(TempIdx);
  // Check if we found a closing parenthesis (indicating the anonymous proc had parameters)
  if TryGetToken(TempIdx, Token) and (Token.ReservedType = rtRightBr) then
    Result := True;
end;

function TCodeFormatterFormatter.FindBeginOrSemicolonAfterProc(out _Offset: Integer; out _Token: TPascalToken): Boolean;
begin
  // search for a 'begin' or semicolon after the procedure name or the closing bracket
  // procedure bla or procedure bla(...)
  //              ^--- here or here ----^
  _Offset := 0;
  repeat
    Assert(False, '.CheckSpecialProcedure: in repeat');
    Inc(_Offset);
    Result := TryGetToken(FTokenIdx + _Offset, _Token);
    if not Result then begin
      // end of file, should not happen with valid code, but this might not be valid code
      Exit; //==>
    end;

    if _Token.ReservedType = rtLeftBr then begin
      // procedure bla( -> there is an opening bracket after the procedure name -> search for the closing bracket
      repeat
        Inc(_Offset);
        Result := TryGetToken(FTokenIdx + _Offset, _Token);
        if not Result then begin
          // end of file, should not happen with valid code, but this might not be valid code
          Exit; //==>
        end;
      until _Token.ReservedType = rtRightBr;
    end;
  until (_Token.ReservedType in [rtSemiColon, rtBegin]);
end;

procedure TCodeFormatterFormatter.CheckSpecialProcedure(var _NoBlankLine, _IsDelegate: Boolean);
var
  Token: TPascalToken;
  Offset: Integer;
begin
  // First check if this is an anonymous procedure by looking at the next non-comment token
  // Anonymous procedures have no name, so the next token is either:
  // - begin (simple anonymous proc)
  // - procedure/function (nested declarations)
  // - var/const/type (local declarations)
  Offset := 0;
  repeat
    Inc(Offset);
    if not TryGetToken(FTokenIdx + Offset, Token) then
      Exit; //==> end of file
  until not (Token.ReservedType in [rtLineFeed, rtComment]);

  if Token.ReservedType in [rtBegin, rtProcedure, rtVar, rtConst, rtType] then begin
    // Anonymous procedure (delegate) - no procedure name, just body or nested declarations
    _IsDelegate := True;
    Exit; //==>
  end;

  // Not an anonymous procedure, use the original logic
  if not FindBeginOrSemicolonAfterProc(Offset, Token) then begin
    // end of file
    Exit; //==>
  end;

  if Token.ReservedType = rtBegin then begin
    // 'begin' before a SemiColon, assume that is a anonymous delegate
    _IsDelegate := True;
    Token.AddOption(toFeedNewLine);
  end else begin
    // semicolon -> Check for 'forward' or 'external'
    repeat
      Inc(Offset);
      if not TryGetToken(FTokenIdx + Offset, Token) then begin
        // end of file
        Exit; //==>
      end;
      if Token.ReservedType in [rtForward, rtExternal] then begin
        _NoBlankLine := True;
        Exit; //==>
      end;
    until not (Token.ReservedType in [rtLineFeed, rtComment, rtSemiColon, rtFuncDirective]);
  end;
end;

procedure TCodeFormatterFormatter.CheckIndentProcedure(var _NTmp: Integer);
var
  Prev1: TPascalToken;
  FunctTypeDeclare, IsDelegate, NoBlankLine: Boolean;
begin
  Assert(False, '.CheckIndentProcedure');
  case FStack.GetTopType of
          // todo: rtClassDecl or rtClass ?
    rtClassDecl, rtRecord: begin
        Assert(False, '.CheckIndentProcedure: Top type is Class or Record');
        FStack.Pop;
        FStack.Push(rtClass, 1);
      end;
    rtVar, rtConst, rtType: begin
              // todo: rtClassDecl or rtClass ?
        if (FPrevToken.ReservedType <> rtEquals)
          and (FStack.GetType(1) in [rtClass, rtClassDecl, rtRecord]) then begin
                // There was a nested class/record declaration that ended
          Assert(False, '.CheckIndentProcedure: Nested Class or Record ended');
          FStack.Pop;
          FStack.Pop;
          FStack.Push(rtClass, 1);
          DecPrevLineIndent;
        end;
      end;
  end;

  FunctTypeDeclare := CheckForFunctTypeDeclare(Prev1);
  NoBlankLine := False;
  IsDelegate := False;

  // Always call CheckSpecialProcedure to detect delegates (including anonymous procedures)
  // For anonymous procedures, FunctTypeDeclare is True (prev token is '('), but we still
  // need to detect if this is a delegate by looking at the next token
  CheckSpecialProcedure(NoBlankLine, IsDelegate);

  if not FunctTypeDeclare and not FIsInInterfacePart and not (FStack.GetTopType = rtClass) then begin
    Assert(False, '.CheckIndentProcedure: not FunctTypeDeclare and not FIsInInterface and not (TopType = rtClass))');
    if not FStack.HasType(rtProcedure) and (FStack.GetTopType <> rtProcDeclare) then begin
      Assert(False, '.CheckIndentProcedure: Stack.HasType(rtProcedure) = False');
      if not IsDelegate then begin
        Assert(False, '.CheckIndentProcedure: not IsDelegate');

        if FSettings.IndentInterfaceImpl and FStack.HasType(rtImplementation) then begin
          // With IndentInterfaceImpl, reset to implementation indent level (1) instead of 0
          Assert(False, '.CheckIndentProcedure: IndentInterfaceImpl, resetting to impl indent');
          if FStack.nIndent > 1 then begin
            FStack.nIndent := 1;
            AddToPrevLineIndent(_NTmp);
          end;
        end else if FStack.nIndent > 0 then begin
          Assert(False, '.CheckIndentProcedure: Stack.nIndent > 0');
          FStack.nIndent := 0;
          AddToPrevLineIndent(_NTmp);
        end;

        FStack.ProcLevel := 0;
        if FSettings.BlankProc and not NoBlankLine then begin
          Assert(False, '.CheckIndentProcedure: Blank Lines Around Proc');
          CheckBlankLinesAroundProc;
        end;
      end;
    end else begin
      Assert(False, '.CheckIndentProcedure: Stack.HasType(rtProcedure) = True');
      // For nested procedures, increment ProcLevel (except when inside anonymous procedure)
      if FStack.GetTopType <> rtProcDeclare then
        FStack.ProcLevel := FStack.ProcLevel + 1;

      if FStack.GetTopType = rtProcDeclare then begin
        Assert(False, '.CheckIndentProcedure: TopType=rtProcDeclare (anonymous proc)');
        // For nested procedures inside anonymous procedures, set indent based on current nIndent
        AddToPrevLineIndent(_NTmp);
        // Then add blank lines if needed (this may change FPrevLine)
        // For anonymous procedures with parameters, always add blank line before nested content
        // Check if the previous non-whitespace token is ')' indicating parameters
        if AnonymousProcHadParams or (FSettings.BlankSubProc and not NoBlankLine) then
          CheckBlankLinesAroundProc;
      end else begin
        // First set indentation, then add blank lines (blank line insertion may change FPrevLine)
        if FStack.nIndent = 0 then begin
          Assert(False, '.CheckIndentProcedure: Stack.nIndent=0');
          AddToPrevLineIndent(_NTmp);
          FStack.nIndent := FStack.nIndent + 1;
        end;

        if FSettings.BlankSubProc and not NoBlankLine then begin
          Assert(False, '.CheckIndentProcedure: Blanc Lines Around Sub Proc');
          CheckBlankLinesAroundProc;
        end;
      end;
    end;

    FStack.Push(rtProcedure, 0);
  end else begin
    // Array of Procedure, Reference To Function...
    if (FStack.GetTopType = rtType) and IsRType(Prev1, [rtOf, rtOper, rtComma]) then begin
      // do nothing
    end else begin
      // todo: rtClass or also rtClassDecl
      if (not FunctTypeDeclare) and not (FStack.GetTopType in [rtClass, rtLeftBr]) then begin
        FStack.nIndent := 0;
        AddToPrevLineIndent(_NTmp);
        if FSettings.IndentComments then
          IndentProcedureComment;
      end;

      // For anonymous functions in assignment context (:=), push with indent
      // increment 1 to capture the continuation indent that would otherwise be
      // lost when begin processes (bug #165)
      if IsRType(Prev1, rtAssignOper) then
        FStack.Push(rtProcDeclare, 1)
      else
        FStack.Push(rtProcDeclare, 0);
    end;
  end;
end;

procedure TCodeFormatterFormatter.CheckIndent(var _NTmp: Integer; var _PrevCommentLine: TLineFeed);
var
  Next: TPascalToken;
  wType: TWordType;
  exp: TGXUnicodeString;
  Idx, IfCol: Integer;
  Token: TPascalToken;
  Depth: Integer;
  IsFirstOnLine: Boolean;
  PoppedPrevSection: Boolean;
  k: Integer;
  IsForLoop: Boolean;
begin
  if FCurrentToken = nil then
    Exit; //==>

  FCurrentRType := FCurrentToken.ReservedType;
  wType := FCurrentToken.WordType;

  if wType in [wtMultilineStringStart, wtMultilineString, wtMultilineStringEnd] then begin
    // we do not indent those
    Exit; //==>
  end;

  // This handles the case where "operator" is not used as a reserved word in "class operator" but
  // as a simple identifier.
  if FCurrentRType = rtClass then begin
    // we currently handle "class", check the next token whether it is "operator"
    if TryGetNextNoComment(FTokenIdx, Next) and (Next.ReservedType = rtOperatorProc) then begin
      // Yes -> handle it like "class procedure"
      Next.SetReservedType(rtProcedure);
    end;
  end;
  if FCurrentRType = rtOperatorProc then begin
    // this can only happen, if the check above for "class operator" did not change
    // this token's ReservedType to rtProcedure
    FCurrentToken.SetReservedType(rtNothing);
    FCurrentRType := rtNothing;
  end;

  // This handles the case where a reserved word was used as the name of a class member.
  // (Is that even allowed?)
  if (FCurrentRType in [rtWhile, rtEnd, rtRepeat, rtBegin, rtUses, rtTry,
      rtProgram, rtType, rtVar, rtConst, rtIf, rtThen, rtElse] + StandardDirectives)
    and IsRType(FPrevToken, rtDot) then begin
    Assert(False, '.CheckIndent: Set ReservedType to rtNothing');
    FCurrentToken.SetReservedType(rtNothing);
    FCurrentRType := rtNothing;
  end;

  { SetSpacing; }
  case FCurrentRType of
    rtIf:
      CheckIndentIf;

    rtThen:
      CheckIndentThen;

    rtColon:
      CheckIndentColon;

    rtElse:
      CheckIndentElse(_NTmp);

    rtRepeat, rtRecord: begin
        FStack.Push(FCurrentRType, 1);
        FWrapIndent := False;
      end;

    rtClass: begin
        if not (TryGetNextNoComment(FTokenIdx, Next)
          and (Next.ReservedType in [rtProcedure, rtProcDeclare, rtOf, rtVar, rtConst])) then begin
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
        // Enable continuation indent for multi-line until conditions (bug #473)
        FWrapIndent := True;
        AddToPrevLineIndent(_NTmp);
      end;

    rtLeftBr:
      CheckIndentLeftBr;

    rtWhile: begin
       // Helper For
        if not IsRType(FPrevToken, rtReserved) then
          FStack.Push(FCurrentRType, 0);
      end;

    rtLeftHook: begin
        // left hook = '['
        Assert(False, '.CheckIndent: LeftHook');
        if IsRType(FPrevToken, rtRightBr) or (IsWType(FPrevToken, wtWord) and not IsRType(FPrevToken, rtOper)) then begin
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
        AddToPrevLineIndent(_NTmp);
        FStack.nIndent := FStack.nIndent + 1;
        FWrapIndent := False;
      end;

    rtVisibility:
      if FStack.GetTopType in [rtClass, rtClassDecl, rtRecord] then begin
        if IsRType(FPrevToken, rtLineFeed) then begin
          DecPrevLineIndent;
          FWrapIndent := False;
        end;
      end else if (FStack.GetTopType in [rtVar, rtConst, rtType]) and (FStack.GetType(1) in [rtClass, rtClassDecl, rtRecord]) then begin
        FStack.Pop;
        DecPrevLineIndent;
        if FStack.GetTopType in [rtClass, rtClassDecl, rtRecord] then begin
          SetPrevLineIndent(FStack.nIndent - 1);
        end;
        FWrapIndent := False;
      end else
        FCurrentToken.SetReservedType(rtNothing);

    rtOf: begin
        // For inline if expressions in case statement, pop the rtElse/rtIfElse markers first
        while FStack.GetTopType in [rtIf, rtThen, rtElse, rtIfElse] do
          FStack.Pop;

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
        // todo: handle procedure/function 'of object' type declaration
      end;

    rtLineFeed: begin
        Assert(False, '.CheckIndent: LineFeed');

        // For inline if expressions, handle vertical formatting
        if FInlineIfDepth > 0 then begin
          // Check if the next token is 'then' or 'else' from an inline if
          if TryGetNextNoComment(FTokenIdx, Next) then begin
            if Next.ReservedType = rtThen then begin
              // Next line will have 'then' - indent it relative to the matching 'if'
              // We need to find the matching 'if' by searching forward from current position
              // Actually, we can't easily do this here because we haven't processed 'then' yet
              // So skip this case - it will be handled in CheckIndentThen
            end;
            // Note: 'else' is handled in CheckIndentElse, not here.
            // Handling it here caused a bug where FPrevToken wasn't updated (due to early Exit),
            // which prevented AdjustSpacing from clearing spBefore on 'else', causing misalignment.
          end;
        end;

        // Check for values on separate lines after then/else in inline if expression
        // We check if the stack top is rtThen/rtElse/rtIfElse pushed with indent 0 (inline if)
        if (FStack.GetTopType in [rtThen, rtElse, rtIfElse]) and (FStack.nIndent = FStack.GetTopIndent) then begin
          // Check if the previous non-comment, non-linefeed token was 'then' or 'else'
          Idx := FTokenIdx - 1;
          while (Idx >= 0) do begin
            Token := GetToken(Idx);
            if (Token.ReservedType <> rtComment) and (Token.ReservedType <> rtLineFeed) then begin
              if Token.ReservedType in [rtThen, rtElse] then begin
                // Value is on a separate line after 'then' or 'else'
                // Find the linefeed before this then/else to get its indentation
                Depth := Idx - 1;
                while (Depth >= 0) and (GetToken(Depth).ReservedType <> rtLineFeed) do
                  Dec(Depth);

                if Depth >= 0 then begin
                  // The then/else keyword is on a line with this indentation
                  // Set the value's indentation to the same + SpacePerIndent
                  IfCol := TLineFeed(GetToken(Depth)).NoOfSpaces;
                  // Subtract 1 to compensate for an off-by-one in how linefeed indentation is calculated
                  TLineFeed(FCurrentToken).NoOfSpaces := IfCol + FSettings.SpacePerIndent - 1;
                end;
                // Don't process the rest of the rtLineFeed logic
                FPrevPrevLine := FPrevLine;
                FPrevLine := TLineFeed(FCurrentToken);
                Exit;
              end else begin
                // Found a non-then/else token, so not a value after then/else
                Break;
              end;
            end;
            Dec(Idx);
          end;
        end;

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
          end else if (FInlineIfDepth > 0) and (FStack.GetTopType in [rtThen, rtElse, rtIfElse]) then begin
            // In inline if context, look past comments AND linefeeds to see if else is coming
            // This ensures comments before 'else' don't get continuation indent
            Idx := FTokenIdx + 1;
            while TryGetToken(Idx, Token) and (Token.ReservedType in [rtComment, rtLineFeed]) do
              Inc(Idx);
            if TryGetToken(Idx, Token) and (Token.ReservedType in [rtElse, rtIfElse]) then begin
              Assert(False, '.CheckIndent: inline if expression else ahead, setting WrapIndent to False');
              FWrapIndent := False;
            end;
          end;

          // In inline if context before 'else', ensure no continuation indent
          if (FInlineIfDepth > 0) and (FStack.GetTopType in [rtThen, rtElse, rtIfElse])
            and (Next.ReservedType = rtLineFeed) then begin
            // Look past comments AND linefeeds to see if else is the next real token
            Idx := FTokenIdx + 1;
            while TryGetToken(Idx, Token) and (Token.ReservedType in [rtComment, rtLineFeed]) do
              Inc(Idx);
            if TryGetToken(Idx, Token) and (Token.ReservedType in [rtElse, rtIfElse]) then
              FWrapIndent := False;
          end;

          // Bug #468: Only apply NoIndentVarDecl when in var/const context, not for if conditions
          if FWrapIndent and not (FSettings.NoIndentVarDecl and (FStack.GetTopType in [rtVar, rtConst])) then begin
            if FSettings.DoubleIndentIfCondition
              and (FStack.GetTopType in [rtIf, rtIfElse]) then begin
              Assert(False, '.CheckIndent: setting NTmp to 2 (double indent if condition)');
              _NTmp := 2;
            end else begin
              Assert(False, '.CheckIndent: setting NTmp to 1');
              _NTmp := 1;
            end;
          end else begin
            Assert(False, '.CheckIndent: setting NTmp to 0');
            _NTmp := 0;
          end;

          if Next.ReservedType in [rtLineFeed] then begin
            // Check if the immediate next token (position+1) is a single-line comment.
            // If so, we're in a sequence of single-line comments and should preserve FWrapIndent.
            // If not (either a true blank line or block comment), reset FWrapIndent.
            // Exception: in case statements (rtOf context), don't preserve FWrapIndent because
            // case selectors should not have extra continuation indent after comments.
            if TryGetToken(FTokenIdx + 1, Next) and (Next.ReservedType = rtComment)
              and (Next.WordType = wtFullComment)
              and not (FStack.GetTopType in [rtOf]) then begin
              // Next immediate token is a single-line // comment, preserve FWrapIndent
              Assert(False, '.CheckIndent: preserving WrapIndent through // comment');
            end else begin
              Assert(False, '.CheckIndent: setting WrapIndent to False');
              FWrapIndent := False;
            end;
          end else if FStack.GetTopType in [rtUses, rtLeftBr] then begin
            Assert(False, '.CheckIndent: setting WrapIndent to False');
            FWrapIndent := False;
          end else if FStack.GetTopType in [rtLeftHook] then begin
            Assert(False, '.CheckIndent: setting WrapIndent to False');
            FWrapIndent := False;
          end else if not (Next.ReservedType in [rtElse, rtIfElse, rtBegin, rtEnd, rtUntil, rtExcept]) then begin
            // Don't set FWrapIndent True if next non-comment token is a control keyword
            // (we already set it False for these at line 2210)
            Assert(False, '.CheckIndent: setting WrapIndent to True');
            FWrapIndent := True;
          end;

          if (Next.ReservedType = rtFuncDirective) and (FStack.GetTopType = rtClass) then begin
            // "Index"  or "Name" can be simple identifiers. That case is handled there but unfortunately
            // that is only after the previous line feed (which is the one handled here)
            // so we detect that case and prevent indenting it
            // todo: Are there any others, e.g. "External" ?)
            if SameText(Next.Content, 'Index') or SameText(Next.Content, 'Name') then begin
              _NTmp := 0;
            end;
          end;
        end;

        FPrevPrevLine := FPrevLine;
        FPrevLine := TLineFeed(FCurrentToken);
        AddToPrevLineIndent(_NTmp);
      end;

    rtAsm: begin
        FormatAsm(_NTmp);
        Exit;
      end;

    rtComma: begin
        if CheckIndentComma() then begin
          // We changed the code, we need to process the comment again
          Exit; //==>
        end;
      end;

    rtProgram, rtUses, rtInitialization:
      if (FCurrentRType = rtUses) and (FStack.GetTopType in [rtProcedure, rtProcDeclare, rtClass]) then begin
        FCurrentToken.SetReservedType(rtNothing);
        FCurrentRType := rtNothing;
      end else begin
        if (FCurrentRType = rtUses) and (FSettings.BlankBeforeUses) then
          CheckBlankLinesBeforeUses;

        // When IndentInterfaceImpl is enabled and we're inside interface/implementation,
        // keep the section indent for uses clause
        if FSettings.IndentInterfaceImpl and (FCurrentRType = rtUses)
          and (FStack.GetTopType in [rtInterface, rtImplementation]) then begin
          // Don't clear the stack or decrement indent - uses should be indented
          // Push with 1 so uses items are indented relative to uses keyword
          FStack.Push(FCurrentRType, 1);
        end else begin
          DecPrevLineIndent;
          FStack.Clear;
          FStack.Push(FCurrentRType, 1);
        end;
        FWrapIndent := False;

        // break after uses
        if (FCurrentRType = rtUses) and (FSettings.FeedAfterUses) then
          AssertLineFeedAfter(FTokenIdx);
      end;

    rtAbsolute:
      if not (FStack.GetTopType in [rtVar, rtConst, rtType]) then begin
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
        if IsRType(FPrevToken, [rtProcedure, rtProcDeclare, rtDot]) then begin
          // the previous token was 'procedure' or '.' -> it's an identifier
          // todo: What if the previous token was a line feed and the one before that was
          //       'procedure' or '.' or if there was a comment in between ?
          FCurrentToken.SetReservedType(rtNothing);
        end else if not (FStack.GetTopType in [rtProcedure, rtProcDeclare, rtClass, rtExternal]) then begin
          // we are not in a procedure or class declaration -> it's an identifier
          FCurrentToken.SetReservedType(rtNothing);
        end else begin
          if TryGetNextNoComment(FTokenIdx, Next)
            and (Next.ReservedType in [rtColon, rtComma]) then begin
            // followed by ':' or ',' -> It is an identifier
            FCurrentToken.SetReservedType(rtNothing);
          end;
        end;
      end;

    rtForward: begin
        Assert(False, '.CheckIndent: rtForward');
        if not (FStack.GetTopType in [rtProcedure, rtProcDeclare]) then begin
          // we are not in a procedure or class declaration -> it's an identifier
          FCurrentToken.SetReservedType(rtNothing);
        end;
      end;

    rtExternal: begin
        Assert(False, '.CheckIndent: rtExternal');
        // There are mulitple options for external declarations:
        // 1. just external (requires a {$L objectfile.obj} directive in the same unit)
        //      procedure MoveWord(var Source, Dest; Count: Integer); external;
        // 2. importing from a library with the function's name:
        //      function SomeFunction(S: string): string; external 'strlib.dll';
        // 3. importing from a library with an explicit name:
        //    function MessageBox(HWnd: Integer; Text, Caption: PChar; Flags: Integer): Integer;
        //      stdcall; external 'user32.dll' name 'MessageBoxA';
        // 4. importing from a library with an index:
        //    function MessageBox(HWnd: Integer; Text, Caption: PChar; Flags: Integer): Integer;
        //      stdcall; external 'user32.dll' index 5;
        // 5. importing from an object file with an explicit name:
        //      procedure FunctionName; cdecl; external object 'ObjectFile.o' name '_FunctionName';
        // 6. importing from a framework:
        //      function foo: Integer; external framework 'framework name';
        // On top of that,the libaray can be delay loaded:
        //   function ExternalMethod(const SomeString: PChar): Integer; stdcall;
        //     external 'cstyle.dll' delayed;
        // and one can declare dependencies:
        //   function DllGetDataSnapClassObject(const [REF] CLSID, [REF] IID: TGUID; var Obj): HResult;
        //     cdecl; external 'libmidas.a' dependency 'stdc++';
        // All these cases have only one thing in common: The external directive ends with a semicolon.
        if FStack.GetTopType in [rtProcedure, rtProcDeclare] then begin
          // we put rtExternal on the stack so we can check for it and remove it when we get to
          // the semicolon
          FStack.Pop;
          FStack.Push(rtProcDeclare, 0);
          FStack.Push(rtExternal, 0);
        end else begin
          // we are not in a procedure or class declaration -> it's an identifier
          FCurrentToken.SetReservedType(rtNothing);
        end;
      end;

    rtProcedure:
      CheckIndentProcedure(_NTmp);

    rtInterface: begin
        if IsRType(FPrevToken, rtEquals) then begin
          // declaration of a OLE object: IClass = interface ['GUID-goes-here']
          FStack.Push(rtClassDecl, 1);
        end else begin
          FIsInInterfacePart := True;
          DecPrevLineIndent;
          if FSettings.IndentInterfaceImpl then
            FStack.Push(rtInterface, 1);
        end;

        FWrapIndent := False;
      end;

    rtImplementation: begin
        FStack.Clear;
        FIsInInterfacePart := False;
        FWrapIndent := False;
        { DecPrevIndent; }
        { nIndent := 0; }
        AddToPrevLineIndent(_NTmp);
        if FSettings.IndentInterfaceImpl then
          FStack.Push(rtImplementation, 1);
      end;

    rtBegin, rtTry:
      CheckIndentBeginTry(_NTmp);

    rtEquals:
      if (FSettings.ShouldAlignVar and (FStack.GetTopType = rtVar))
        or (FSettings.ShouldAlignConst and (FStack.GetTopType = rtConst)) then
        FCurrentToken := AlignExpression(FTokenIdx, FSettings.AlignVarPos);

    rtAssignOper:
      if FSettings.AlignAssign
        and not (FStack.GetTopType in [rtVar, rtConst]) then begin
        // Exclude for-loop: scan backwards past identifiers, dots, 'var',
        // whitespace, comments, and compiler directives to find 'for' (rtWhile)
        IsForLoop := False;
        k := 1;
        while TryGetToken(FTokenIdx - k, Token) do begin
          if Token.ReservedType = rtWhile then begin // 'for' is mapped to rtWhile
            IsForLoop := True;
            Break;
          end;
          if Token.ReservedType in [rtLineFeed, rtComment, rtCompIf, rtCompElse,
            rtCompEndif, rtDot, rtVar, rtColon] then begin
            Inc(k);
            Continue;
          end;
          if Token.WordType in [wtWord, wtCompDirective] then begin
            Inc(k);
            Continue;
          end;
          Break;
        end;
        if not IsForLoop then
          FCurrentToken := AlignExpression(FTokenIdx, FSettings.AlignAssignPos);
      end;

    rtVar, rtConst, rtType:
      if not (FStack.GetTopType in [rtLeftBr, rtLeftHook]) then begin
        FWrapIndent := False;
        if FStack.nIndent < 1 then
          FStack.nIndent := 1;
        PoppedPrevSection := False;
        if (FStack.GetTopType in [rtVar, rtConst, rtType]) then begin
          if (FCurrentRType = rtType) and IsRType(FPrevToken, rtEquals) then begin
            // in classes.pas I found
            // t = type AnsiString
            FStack.Pop
          end else if FStack.GetType(1) in [rtClass, rtClassDecl, rtRecord] then begin
            FStack.Pop;
            DecPrevLineIndent;
          end else begin
            FStack.Pop;
            PoppedPrevSection := True;
          end;
        end;
        if (FStack.GetTopType in [rtClass, rtClassDecl, rtRecord]) then begin
          if IsRType(FPrevToken, rtVisibility) then begin
            FStack.Push(FCurrentRType, 0);
          end else begin
            FStack.Push(FCurrentRType, 1);
          end;
        end else begin
          if (FStack.GetTopType = rtProcDeclare) then begin
            // inline function
            FStack.Push(FCurrentRType, 1);
            if not IsRType(FPrevToken, rtEquals) then begin
              if FSettings.FeedAfterVar then
                AssertLineFeedAfter(FTokenIdx);
            end;
          end else begin
            if ((FPrevToken is TLineFeed) and (FStack.GetTopType in [rtBegin, rtTry])) then begin
              // inline var declaration
            end else if (FPrevToken.TryGetExpression(exp) and SameText(exp, 'for')) then begin
              // for with inline var declaration
            end else if FSettings.IndentInterfaceImpl
              and (FStack.GetTopType in [rtInterface, rtImplementation]) then begin
              // Unit level type/var/const with IndentInterfaceImpl enabled:
              // Only decrement if we popped a previous section (transition from type->var etc.)
              if PoppedPrevSection then
                DecPrevLineIndent;
              // Push with 1 so content is indented relative to keyword
              FStack.Push(FCurrentRType, 1);
              if FSettings.FeedAfterVar then
                AssertLineFeedAfter(FTokenIdx);
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
        // For inline if expressions in for/while loop, pop the rtElse/rtIfElse markers first
        while FStack.GetTopType in [rtIf, rtThen, rtElse, rtIfElse] do
          FStack.Pop;

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
              if (FLastPopResType in [rtOn, rtWhile]) and FSettings.FeedAfterThenExceptSingle then
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
          AddToPrevLineIndent(_NTmp);

        if NoBeginTryIndent(FCurrentRType) then
          FStack.nIndent := FStack.nIndent + 1;
      end;

    rtComment: begin
        // Only reset FWrapIndent for block comments (wtHalf*), not for single-line // comments.
        // This preserves continuation indent through sequences of single-line comments.
        // Exception: in case statements (rtOf), always reset FWrapIndent so case selectors align properly.
        if FSettings.IndentComments and not (FStack.GetTopType in [rtLeftHook, rtRecord, rtClassDecl])
          and ((FCurrentToken.WordType in [wtHalfComment, wtHalfStarComment, wtHalfOutComment])
            or (FStack.GetTopType in [rtOf])) then
          FWrapIndent := False;

        if FStack.IsEmpty and (FStack.nIndent > 0) then begin
          FStack.nIndent := 0;
          AddToPrevLineIndent(_NTmp);
        end;

        AdjustSpacing(GetToken(FTokenIdx + 1), FCurrentToken, FTokenIdx + 1);
        if (FPrevLine <> nil) and (FPrevLine = FPrevToken) then begin
          // Check if we're in a inline if context and 'else' is coming - if so, don't restore old indent
          IsFirstOnLine := False;
          if (FInlineIfDepth > 0) and (FStack.GetTopType in [rtThen, rtElse, rtIfElse]) then begin
            Idx := FTokenIdx + 1;
            while TryGetToken(Idx, Token) and (Token.ReservedType in [rtComment, rtLineFeed]) do
              Inc(Idx);
            if TryGetToken(Idx, Token) and (Token.ReservedType in [rtElse, rtIfElse]) then
              IsFirstOnLine := True; // Reuse this variable to indicate inline if else context
          end;
          if (not FSettings.IndentComments
            or (FCurrentToken.WordType in [wtFullOutComment, wtHalfOutComment]))
            and not IsFirstOnLine then
            FPrevLine.NoOfSpaces := FPrevLine.OldNoOfSpaces
          else begin
            if Assigned(_PrevCommentLine) then
              FPrevLine.NoOfSpaces := FPrevLine.OldNoOfSpaces +
                (_PrevCommentLine.NoOfSpaces - _PrevCommentLine.OldNoOfSpaces)
            else
              _PrevCommentLine := FPrevLine;
          end;
        end else if FSettings.AlignComments and (FCurrentToken.WordType = wtFullComment) then begin
          if TryGetToken(FTokenIdx + 1, Next) and (Next.ReservedType = rtLineFeed) then
            FCurrentToken := AlignExpression(FTokenIdx, FSettings.AlignCommentPos);
        end;
      end;

    rtSemiColon: begin
        if CheckIndentSemicolon() then
          Exit; //==>
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
    Assert(False, '.CheckIndent: not (rtLineFeed or rtComment)');
    _PrevCommentLine := nil;
    Assert(False, '.CheckIndent: _PrevCommentLine=nil');
  end else begin
    if not Assigned(_PrevCommentLine) then
      Assert(False, '.CheckIndent: _PrevCommentLine=nil')
    else
      Assert(False, Format('.CheckIndent: _PrevCommentLine.NoOfSpaces=%d .OldNoOfSpaces=%d',
        [_PrevCommentLine.NoOfSpaces, _PrevCommentLine.OldNoOfSpaces]));
  end;

  if wType = wtCompDirective then begin
    Assert(False, '.CheckIndent: compiler directive');
    FWrapIndent := False;

    // Handle indentation for directives at the start of a line
    if IsRType(FPrevToken, rtLineFeed) then begin
      case FSettings.IndentCompDirectives of
        icdNone: begin
          // Reset to column 1
          Assert(False, '.CheckIndent: IndentCompDirectives=icdNone');
          _NTmp := -FStack.nIndent;
          Assert(False, Format('NTmp=%d', [_NTmp]));
          AddToPrevLineIndent(_NTmp);
        end;
        icdIndependent: begin
          // Indent based on directive nesting level
          // Note: nIndent is in terms of indent levels, not spaces. SetIndent multiplies by SpacePerIndent.
          Assert(False, Format('.CheckIndent: IndentCompDirectives=icdIndependent, NestLevel=%d', [FCompDirectiveNestLevel]));
          case FCurrentRType of
            rtCompIf: begin
              _NTmp := -FStack.nIndent + FCompDirectiveNestLevel;
            end;
            rtCompElse: begin
              _NTmp := -FStack.nIndent + (FCompDirectiveNestLevel - 1);
            end;
            rtCompEndif: begin
              _NTmp := -FStack.nIndent + (FCompDirectiveNestLevel - 1);
            end;
          else
            // Other directives ({$DEFINE}, {$UNDEF}, {$MESSAGE}, etc.) - indent at current level
            _NTmp := -FStack.nIndent + FCompDirectiveNestLevel;
          end;
          Assert(False, Format('NTmp=%d', [_NTmp]));
          AddToPrevLineIndent(_NTmp);
        end;
        icdFollowCode: begin
          // Follow code indentation - don't adjust
          Assert(False, '.CheckIndent: IndentCompDirectives=icdFollowCode');
        end;
      end;
    end;

    // Track nesting level for ALL directives (including inline ones)
    // This must happen regardless of whether the directive is at the start of a line
    if FSettings.IndentCompDirectives = icdIndependent then begin
      case FCurrentRType of
        rtCompIf:
          Inc(FCompDirectiveNestLevel);
        rtCompEndif:
          if FCompDirectiveNestLevel > 0 then
            Dec(FCompDirectiveNestLevel);
      end;
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
  PrevCommentLine: TLineFeed;
begin
  FTokens := _Tokens;

  if FSettings.ChangeIndent then begin
    FPrevLine := nil;
    FPrevPrevLine := nil;
    FPrevToken := nil;
    FWrapIndent := True;
    FIsInInterfacePart := False;
    NTmp := 0;
    PrevCommentLine := nil;
    // the StackStack is used to preserve indenting over IFDEF/ELSE/ENDIF statements
    FStackStack := TCodeFormatterStack.Create;
    try
      FTokenIdx := 0;
      while TryGetToken(FTokenIdx, FCurrentToken) do begin
        Assert(False, '**** .doExecute: CurrentToken(' + IntToStr(FTokenIdx) + '): ' + FCurrentToken.GetForDebug);
        Assert(False, '.doExecute: Stack.Depth: ' + IntToStr(FStack.Depth) + ' .TopType: ' + GetEnumname(TypeInfo(TReservedType), Ord(FStack.GetTopType)) + ' .TopIndent: ' + IntToStr(FStack.GetTopIndent));
        Assert(False, '.doExecute: WrapIndent: ' + Ifthen(FWrapIndent, 'True', 'False'));
        Assert(False, '.doExecute: before CheckIndent: NTmp: ' + IntToStr(NTmp));
        if not Assigned(PrevCommentLine) then begin
          Assert(False, '.doExecute: before CheckIndent: PrevCommentLine=nil');
        end else begin
          Assert(False, Format('.doExecute: before CheckIndent: PrevCommentLine.NoOfSpaces=%d .OldNoOfSpaces=%d',
            [PrevCommentLine.NoOfSpaces, PrevCommentLine.OldNoOfSpaces]));
        end;
        CheckIndent(NTmp, PrevCommentLine);
//        FTokens.WriteTo(Format('d:\output%.3d.txt', [FTokenIdx]));
        Assert(False, '.doExecute: after CheckIndent:  NTmp: ' + IntToStr(NTmp));
        if not Assigned(PrevCommentLine) then begin
          Assert(False, '.doExecute: after CheckIndent: PrevCommentLine=nil');
        end else begin
          Assert(False, Format('.doExecute: after CheckIndent: PrevCommentLine.NoOfSpaces=%d .OldNoOfSpaces=%d',
            [PrevCommentLine.NoOfSpaces, PrevCommentLine.OldNoOfSpaces]));
        end;
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

function TCodeFormatterFormatter.InsertLinefeedBefore(ATokenIdx: Integer): Boolean;
var
  PrevPrevLine: TLineFeed;
begin
  PrevPrevLine := FPrevLine;
  FPrevLine := TLineFeed.Create(PrevPrevLine.OldNoOfSpaces, FSettings.SpacePerIndent, -1);
  FPrevLine.NoOfSpaces := PrevPrevLine.NoOfSpaces;
  FPrevLine.Wrapped := True;
  GetToken(ATokenIdx).SetSpace([spBefore], False);
  FTokens.AtInsert(ATokenIdx, FPrevLine);
  Result := True;
end;

function TCodeFormatterFormatter.IsInlineIfExpression: Boolean;
var
  Idx: Integer;
  Token: TPascalToken;
  Expr: TGXUnicodeString;
begin
  // Detect if 'if' is used as an inline if expression rather than a statement
  // by looking at the previous non-comment, non-linefeed token
  Result := False;

  // If we're already inside an inline if (FInlineIfDepth > 0),
  // then this is a nested inline if
  if FInlineIfDepth > 0 then begin
    Result := True;
    Exit;
  end;

  // Start from previous token
  Idx := FTokenIdx - 1;

  // Skip backwards over comments and linefeeds to find the actual previous token
  while (Idx >= 0) do begin
    Token := GetToken(Idx);
    if Token.ReservedType = rtComment then begin
      Dec(Idx);
      Continue;
    end;
    if Token.ReservedType = rtLineFeed then begin
      Dec(Idx);
      Continue;
    end;

    // Found the previous significant token, now check its type
    case Token.ReservedType of
      // Expression operators that indicate inline if context
      rtAssignOper,  // :=
      rtEquals,      // =
      rtLeftBr,      // (
      rtComma,       // ,
      rtLeftHook,    // [
      rtPlus,        // +
      rtMinus,       // -
      rtMathOper,    // *, /, div, mod
      rtLogOper,     // <, >, <=, >=, <>, and, or, xor
      rtCase,        // case <expression> of
      rtIf:          // if <expression> - second 'if' in 'if if ...' is inline if
        Result := True;

      // 'else if' is handled by not decrementing FInlineIfDepth in CheckIndentElse
      // when the next token is 'if', so no special handling needed here

      rtOper: begin
        // Check for other operators like 'to', 'downto', 'of', 'in', 'is', 'as', 'not'
        if Token.TryGetExpression(Expr) then begin
          Expr := LowerCase(Expr);
          if (Expr = 'to') or (Expr = 'downto') or (Expr = 'of')
            or (Expr = 'in') or (Expr = 'is') or (Expr = 'as')
            or (Expr = 'not') then
            Result := True;
        end;
      end;
    end;

    // We found a significant token and checked it, so exit loop
    Exit;
  end;

  // If we reach here, there was no previous token (start of file)
  // which means it's a statement if
  Result := False;
end;

function TCodeFormatterFormatter.GetTokenColumn(_TokenIdx: Integer): Integer;
var
  Idx: Integer;
  Token: TPascalToken;
  Col: Integer;
  LineFeedIdx: Integer;
  Expr: TGXUnicodeString;
  RType: TReservedType;
  NextToken: TPascalToken;
  NextRType: TReservedType;
begin
  // Calculate the column position where a token appears in the final output.
  // We can't rely on spBefore/spAfter flags because they may be modified
  // during formatting (e.g., spAfter is cleared to avoid double spaces).
  // Instead, calculate based on token text lengths plus spaces between tokens,
  // but skip spaces for brackets and similar tokens that don't have spaces.

  Col := 0;
  LineFeedIdx := -1;

  // Search backwards from token to find the line feed
  Idx := _TokenIdx - 1;
  while Idx >= 0 do begin
    Token := GetToken(Idx);
    if Token is TLineFeed then begin
      LineFeedIdx := Idx;
      Col := TLineFeed(Token).NoOfSpaces;
      Break;
    end;
    Dec(Idx);
  end;

  // If no line feed found, we're on the first line, start at column 0
  if LineFeedIdx < 0 then
    LineFeedIdx := -1;

  // Count tokens between linefeed and target, calculating their actual widths
  Idx := LineFeedIdx + 1;
  while Idx < _TokenIdx do begin
    Token := GetToken(Idx);
    if not (Token is TLineFeed) then begin
      // Add the token's text length
      if Token.TryGetExpression(Expr) then
        Inc(Col, Length(Expr))
      else
        Inc(Col); // Fallback for non-expression tokens

      // Add space after this token, but not for brackets and not before right brackets
      RType := Token.ReservedType;
      if Idx + 1 < FTokens.Count then begin
        NextToken := GetToken(Idx + 1);
        NextRType := NextToken.ReservedType;
      end else begin
        NextRType := rtNothing;
      end;

      // Don't add space after left brackets, or before right brackets
      // Also don't add space before left bracket when current token is an identifier
      // (function calls like "ShowMessage(" have no space, but "then (" does)
      if not (RType in [rtLeftBr, rtLeftHook]) and
         not (NextRType in [rtRightBr, rtRightHook]) and
         not ((NextRType in [rtLeftBr, rtLeftHook]) and (RType in [rtNothing, rtReserved])) then
        Inc(Col);
    end;
    Inc(Idx);
  end;

  Result := Col;
end;

function TCodeFormatterFormatter.FindMatchingInlineIf: Integer;
var
  Idx: Integer;
  Token: TPascalToken;
  Depth: Integer;
  RType: TReservedType;
  FoundMatchingThen: Boolean;
begin
  // Find the corresponding 'if' for the current 'else' in an inline if expression
  // Structure: if COND then TRUE else FALSE
  // When searching backwards from 'else':
  //  1. Skip FALSE value (might contain nested ternaries)
  //  2. Find the matching 'then' (the one at depth 0)
  //  3. Skip COND (might contain nested ternaries)
  //  4. Find the 'if'
  Result := -1;
  Depth := 0;
  FoundMatchingThen := False;

  // Start from current token and search backwards
  Idx := FTokenIdx - 1;
  while Idx >= 0 do begin
    Token := GetToken(Idx);
    RType := Token.ReservedType;

    // Skip comments and line feeds
    if (RType = rtComment) or (RType = rtLineFeed) then begin
      Dec(Idx);
      Continue;
    end;

    if not FoundMatchingThen then begin
      // Phase 1: Looking for the matching 'then'
      if RType = rtElse then begin
        // Entering a nested inline if in the TRUE part
        Inc(Depth);
      end
      else if RType = rtThen then begin
        if Depth = 0 then begin
          // Found the matching 'then'!
          FoundMatchingThen := True;
        end else begin
          // Exiting a nested inline if
          Dec(Depth);
        end;
      end;
    end else begin
      // Phase 2: Looking for the matching 'if' (after finding 'then')
      if RType = rtIf then begin
        // Found the matching 'if'!
        Result := Idx;
        Exit;
      end;
      // Skip over condition tokens (including nested ternaries in the condition)
    end;

    Dec(Idx);
  end;
end;

procedure TCodeFormatterFormatter.CheckWrapping;
var
  HasInserted: Boolean;
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
          HasInserted := InsertLinefeedBefore(k + 1);
          TokenIdx := j;
        end;

        if (K2 = 0) and (GetToken(k).Space(spAfter)
          or GetToken(k + 1).Space(spBefore)) then
          K2 := k + 1;

        Dec(k);
      end;

      if not HasInserted and (K2 <> 0) and (K2 > j) then begin
        InsertLinefeedBefore(K2);
        TokenIdx := j;
      end;

      LineLen := 0;
    end;
    Inc(TokenIdx);
  end;
end;

end.
