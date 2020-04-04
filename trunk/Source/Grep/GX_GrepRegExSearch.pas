unit GX_GrepRegExSearch;

interface

{$I GX_CondDefine.inc}

uses
  SysUtils, Classes, RegExpr, GX_GenericUtils, GX_CodeFormatterUnicode, GX_OtaUtils;

type
  TFoundEvent = procedure(LineNo, StartCol, EndCol: Integer; const Line: TGXUnicodeString) of object;

  TSearcher = class(TObject)
  private
    FPattern: TGXUnicodeString;
    FData: TGXUnicodeStringList;
    FOnFound: TFoundEvent;
    FRegEx: TRegExpr;
    FCaseSensitive: Boolean;
    FWholeWord: Boolean;
    FRegularExpression: Boolean;
    FFileName: string;
    FNoCode: Boolean;
    FNoStrings: Boolean;
    FNoComments: Boolean;
    FIsPascalSourceFile: Boolean;
    FSectionInterface: Boolean;
    FSectionInitialization: Boolean;
    FSectionImplementation: Boolean;
    FSectionFinalization: Boolean;
    FHandleMultilineInDfm: Boolean;
    FHandleSpecialCharasInDfm: Boolean;
    procedure SearchLineRegEx(const LineStr: string; LineNo, Offset: Integer);
    procedure SearchLineRaw(const LineStr: string; LineNo, Offset: Integer);
    procedure SetFileName(const Value: string);
    function CheckWholeWord(Line: TGXUnicodeString; StartCol, EndCol: Integer): Boolean;
    procedure SearchForm;
    procedure SearchPascalFile;
    procedure SearchLine(_Line: TGXUnicodeString; _LineIdx, _Offset: Integer);
    procedure SearchTextFile;
  public
    constructor Create;
    destructor Destroy; override;
    property Pattern: TGXUnicodeString read FPattern write FPattern;
    property OnFound: TFoundEvent read FOnFound write FOnFound;
    property NoCode: Boolean read FNoCode write FNoCode;
    property NoStrings: Boolean read FNoStrings write FNoStrings;
    property NoComments: Boolean read FNoComments write FNoComments;
    property IsPascalSourceFile: Boolean read FIsPascalSourceFile write FIsPascalSourceFile;
    property SectionInterface: Boolean read FSectionInterface write FSectionInterface;
    property SectionImplementation: Boolean read FSectionImplementation write FSectionImplementation;
    property SectionInitialization: Boolean read FSectionInitialization write FSectionInitialization;
    property SectionFinalization: Boolean read FSectionFinalization write FSectionFinalization;
    property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive;
    property HandleMultilineInDfm: Boolean read FHandleMultilineInDfm write FHandleMultilineInDfm;
    property HandleSpecialCharasInDfm: Boolean read FHandleSpecialCharasInDfm write FHandleSpecialCharasInDfm; 
    property RegularExpression: Boolean read FRegularExpression write FRegularExpression;
    property WholeWord: Boolean read FWholeWord write FWholeWord;
    property FileName: string read FFileName write SetFileName;
    procedure Execute; overload;
  end;

implementation

uses
  u_dzStringUtils;

const
  CReplacementChar: TGXUnicodeChar = #1; // don't match false-positives when searcing for spaces

{ TSearcher }

function TSearcher.CheckWholeWord(Line: TGXUnicodeString; StartCol, EndCol: Integer): Boolean;
var
  PrevChar: TGXUnicodeChar;
  NextChar: TGXUnicodeChar;
  FirstMatchChar: TGXUnicodeChar;
  LastMatchChar: TGXUnicodeChar;
begin
  PrevChar := ' ';
  NextChar := ' ';
  if StartCol >= 2 then
    PrevChar := Line[StartCol - 1];
  if Length(Line) > EndCol then
    NextChar := Line[EndCol + 1];
  FirstMatchChar := Line[StartCol];
  LastMatchChar := Line[EndCol];
  Result := (((not IsCharIdentifier(PrevChar)) or (not IsCharIdentifier(FirstMatchChar)))
    and ((not IsCharIdentifier(NextChar)) or (not IsCharIdentifier(LastMatchChar))));
end;

constructor TSearcher.Create;
begin
  FRegEx := TRegExpr.Create;
end;

destructor TSearcher.Destroy;
begin
  FreeAndNil(FRegEx);
  inherited;
end;

type
  TCodeFragment = (
    cfCode,
    cfLineComment, // This type of comment, ends on newline.
    cfCurlyComment, // {...newlines...}
    cfBlockComment, // (*...newlines...*)
    cfString, // = '...'
    // cfEndOfCode, TODO : Switch to this after usEnd
    cfEndOfComment, // Last char of comment; Either #10, ')' or '}'
    cfEndOfString // Last char of string; Always '
    );
  TCodeFragments = set of TCodeFragment;

  TUnitSection = (
    usStart,
    usInterface, usImplementation, usInitialization, usFinalization,
    usEnd);
  TUnitSections = set of TUnitSection;

const
  CodeFragments = [cfCode];
  StringFragments = [cfString, cfEndOfString];
  CommentFragments = [cfLineComment, cfCurlyComment, cfBlockComment, cfEndOfComment];
  CSections: array [TUnitSection] of string = (
    '',
    'interface', 'implementation', 'initialization', 'finalization',
    'end.');

procedure TSearcher.SearchLine(_Line: TGXUnicodeString; _LineIdx, _Offset: Integer);
begin
  if RegularExpression then
    SearchLineRegEx(_Line, _LineIdx, _Offset)
  else
    SearchLineRaw(_Line, _LineIdx, _Offset);
end;

procedure TSearcher.SearchForm;
var
  PrevLine: TGXUnicodeString;

  procedure doSearchLine(_Line: TGXUnicodeString; _LineIdx: Integer);
  var
    p: Integer;
    s: TGXUnicodeString;
    Line: TGXUnicodeString;
    CodeStr: TGXUnicodeString;
    Code: Integer;
    Offset: Integer;
  begin
    if not FHandleSpecialCharasInDfm then begin
      SearchLine(_Line, _LineIdx, 0);
      Exit; //==>
    end;

    Line := '';
    Offset := 0;
    s := _Line;
    p := Pos('''#', s);
    while p > 0 do begin
      Line := Line + Copy(s, 1, p - 1);
      s := Copy(s, p+1);
      p := Pos('''', s);
      CodeStr := Copy(s, 2, p - 2);
      s := Copy(s, p + 1);
      if TryStrToInt(CodeStr, Code) then begin
        Line := Line + TGXUnicodeChar(Code);
        Inc(Offset, p);
      end else
        Line := Line + '#' + CodeStr;
      p := Pos('''#', s);
    end;
    Line := Line + s;
    SearchLine(Line, _LineIdx, Offset);
  end;

  procedure HandleFormLine(_Line: TGXUnicodeString; _CurrLineIdx: Integer; var _LineIdx: Integer);
  var
    s: TGXUnicodeString;
    Len: Integer;
    i: Integer;
    Prefix: TGXUnicodeString;
  begin
    if not FHandleMultilineInDfm then begin
      doSearchLine(_Line, _CurrLineIdx);
      Exit //==>
    end;

    // There are two special cases here:
    // 1. A string can be split into multlple lines.
    //    This usually looks like this:
    //      Caption =
    //        'first line with some wor' +
    //        'ds and the second line'
    //    Note that the line break can happen within a word.
    // 2. Special characters (e.g. Umlauts or control characters) can be embedded with #<number>
    //      Caption = 'Kurze '#220'berschrift'
    //
    // The special characters are handled in the sub proedure doSearchLine
    //
    // The following is an attempt to handle the multiline case.
    // It seems to work, but it breaks the preview window in the Grep results.
    Prefix := '';
    i := 1;
    Len := Length(_Line);
    while (i <= Len) and (_Line[i] = ' ') do begin
      Inc(i);
      Prefix := Prefix + ' ';
    end;
      
    s := Trim(_Line);
    Len := Length(s);
    if (PrevLine <> '') then begin
      if (Copy(s, 1, 1) = '''') and
        ((Copy(s, Len, 1) = '''') or (Copy(s, Len - 1, 2) = ''')')) then begin
        // the second is the special case for the last line in a string list property (e.g TMemo.Lines)
        // We have already collected a part of the string, add the last part to it
        PrevLine := Copy(PrevLine, 1, Length(PrevLine) - 1) + Copy(s, 2, Len - 1);
        // the string ends here (because there is no "' +" at the end), so let's do the search
        doSearchLine(PrevLine, _LineIdx);
        PrevLine := '';
        // we're done
        Exit; //==>
      end;
    end;

    if (Copy(s, 1, 1) = '''') and (Copy(s, Len - 2, 3) = ''' +') then begin
      // this line has a continuation in the next line
      if PrevLine = '' then begin
        // It is the first of these
        PrevLine := Prefix + Copy(s, 1, Len - 2);
        _LineIdx := _CurrLineIdx;
      end else begin
        // It is itself a continuation of a previous line
        PrevLine := Copy(PrevLine, 1, Length(PrevLine) - 1) + Copy(s, 2, Length(s) - 3);
      end;
      // This line will be searched together with the previous one once we have the full string
      Exit; //==>
    end;

    if PrevLine <> '' then begin
      // This is a new line, so if we have deferred searching a previous line, we search it now.
      // This should never happen, really since the end of the string should have been
      // handled in the first if case above, the one where PrevLine is set to '' before the Exit.
      doSearchLine(PrevLine, _LineIdx);
      PrevLine := '';
    end;

    doSearchLine(_Line, _CurrLineIdx);
  end;

var
  StartLineIdx: Integer;
  LineIdx: Integer;
  Line: TGXUnicodeString;
begin
  //ActiveCodeFragment := cfCode;
  PrevLine := '';
  StartLineIdx := 0;
  for LineIdx := 0 to FData.Count - 1 do
  begin
    // Read the input line, skip if empty.
    Line := FData[LineIdx];
    if Line = EmptyString then begin
      PrevLine := '';
      Continue;
    end;

    HandleFormLine(Line, LineIdx, StartLineIdx);
  end;

  // if there is a string left in PrevLine, we must handle it too
  if PrevLine <> '' then
    doSearchLine(PrevLine + '''', FData.Count - 1);
end;

procedure TSearcher.SearchPascalFile;
var
  ActiveSection: TUnitSection;

  function _TrySwitchToSection(const _Line: TGXUnicodeString; const _LineIdx: Integer;
    const aNextSection: TUnitSection): Boolean;
  begin
    if ActiveSection < aNextSection then
      if Length(_Line) >= _LineIdx + Length(CSections[aNextSection]) then
        if StrLIComp(@(_Line[_LineIdx]), CSections[aNextSection], Length(CSections[aNextSection])) = 0 then
          if (aNextSection = usEnd) or (not IsCharIdentifier(_Line[_LineIdx + Length(CSections[aNextSection])])) then begin
            ActiveSection := aNextSection;
            Result := True;
            Exit;
          end;

    Result := False;
  end; // _TrySwitchToSection

var
  i, index2: Integer;
  Line: TGXUnicodeString;
  Index: Integer;
  UnitSectionsToSkip: TUnitSections;
  CodeFragmentsToSkip: TCodeFragments;
  ActiveCodeFragment: TCodeFragment;
begin // SearchPascalFile
  UnitSectionsToSkip := [];
  CodeFragmentsToSkip := [];

  // Determine which sections must be skipped :
  if not SectionInterface then Include(UnitSectionsToSkip, usInterface);
  if not SectionImplementation then Include(UnitSectionsToSkip, usImplementation);
  if not SectionInitialization then Include(UnitSectionsToSkip, usInitialization);
  if not SectionFinalization then Include(UnitSectionsToSkip, usFinalization);

  // Determine which code fragments must be skipped :
  if NoCode then CodeFragmentsToSkip := CodeFragmentsToSkip + CodeFragments;
  if NoStrings then CodeFragmentsToSkip := CodeFragmentsToSkip + StringFragments;
  if NoComments then CodeFragmentsToSkip := CodeFragmentsToSkip + CommentFragments;

  ActiveSection := usStart;
  ActiveCodeFragment := cfCode;
  for i := 0 to FData.Count - 1 do
  begin
    // Read the input line, skip if empty.
    Line := FData[i];
    if Line = EmptyString then begin
      Continue;
    end;

    if (UnitSectionsToSkip <> []) or (CodeFragmentsToSkip <> []) then
    begin
      Assert(Length(Line) > 0);
      // On each new line, switch line comment mode back to code :
      if ActiveCodeFragment = cfLineComment then
        ActiveCodeFragment := cfCode;

      Line := Line + CReplacementChar; // Extend the input line to avoid out of bounds errors.
      index := 1;
      // This (and prior) code scans Delphi syntax. TODO : Handle IsC and IsDFM syntax too.
      repeat
        Assert(Line[index] <> #10, 'unexpected linefeed');
        Assert(Line[index] <> #13, 'unexpected carriage return');

        index2 := index + 1;
        case ActiveCodeFragment of
          cfLineComment: ; // skip everything, active fragment switches back to code on the next line
          cfCurlyComment: if Line[index] = '}' then ActiveCodeFragment := cfEndOfComment;
          cfBlockComment:
            if Line[index] = '*' then
              if Line[index2] = ')' then
              begin
                Inc(index2); // skip both closing characters
                ActiveCodeFragment := cfEndOfComment;
              end;
          cfString: // already seen one '
            if Line[index] = #39 then
              // Is this single quote followed by another?
              if Line[index2] = #39 then
                Inc(index2) // skip double-escaped single quote character
              else
                ActiveCodeFragment := cfEndOfString;
        else // cfCode, cfEndOfString, cfEndOfComment:
          ActiveCodeFragment := cfCode;
          case Line[index] of
            #39: ActiveCodeFragment := cfString;
            '{': ActiveCodeFragment := cfCurlyComment;
            '/': if Line[index2] = '/' then ActiveCodeFragment := cfLineComment;
            '(': if Line[index2] = '*' then ActiveCodeFragment := cfBlockComment;
          end; // case s[index]
        end; // case ActiveCodeFragment

        // Do we need to detect unit section-changes?
        if UnitSectionsToSkip <> [] then
          if ActiveCodeFragment = cfCode then
            if ActiveSection < usEnd then
              // Is this the start of an identifier (looking back in the UNALTERED line)?
              if (index = 1)
              or ((not IsCharIdentifier(FData[i][index - 1])) and (FData[i][index - 1] <> '&')) then
                if _TrySwitchToSection(Line, Index, usInterface)
                or _TrySwitchToSection(Line, Index, usImplementation)
                or _TrySwitchToSection(Line, Index, usInitialization)
                or _TrySwitchToSection(Line, Index, usFinalization)
                or _TrySwitchToSection(Line, Index, usEnd) then
                  // When detected, step over the activated section keyword :
                  Inc(index2, Length(CSections[ActiveSection]) - 1);

        // Lastly, put a CReplacementChar over fragments and sections that must not be searched :
        if (ActiveSection in UnitSectionsToSkip)
        or (ActiveCodeFragment in CodeFragmentsToSkip) then
          repeat
            Line[index] := CReplacementChar;
            Inc(index);
          until index = index2
        else
          index := index2;
      until index >= Length(Line); // Stop at the extra character we added

      // To here, s has had all content removed that must not be searched.
      // For the search, we trim all trailing CReplacementChar's out of s.
      while (index > 0) and (Line[index] = CReplacementChar) do
        Dec(index);

      // If s is empty we skip the search.
      if index = 0 then
        Continue;

      SetLength(Line, index);
    end;
    SearchLine(Line, i, 0);
  end; // for i FData
end;


procedure TSearcher.SearchTextFile;
var
  i: Integer;
  Line: TGXUnicodeString;
begin
  for i := 0 to FData.Count - 1 do begin
    Line := FData[i];
    if Line <> EmptyString then begin
      SearchLine(Line, i, 0);
    end;
  end;
end;

procedure TSearcher.Execute;
begin
  if not Assigned(FData) then
    raise Exception.Create('Data to search not provided');
  if Pattern = '' then
    raise Exception.Create('Search pattern is empty');

  if RegularExpression then
  begin
    FRegEx.ModifierI := not CaseSensitive;
    FRegEx.Expression := Pattern;
    FRegEx.Compile;
  end;

  if IsForm(FileName) then
    SearchForm
  else if IsPascalSourceFile then
    SearchPascalFile
  else
    SearchTextFile;
end;

procedure TSearcher.SearchLineRaw(const LineStr: string; LineNo, Offset: Integer);
var
  StartCol: Integer;
  EndCol: Integer;
  Line: TGXUnicodeString;
  StartIndex: Integer;
  MatchPos: Integer;

  procedure GetNextMatch;
  begin
    if CaseSensitive then
      MatchPos := PosFrom(Pattern, LineStr, StartIndex)
    else
      MatchPos := CaseInsensitivePosFrom(Pattern, LineStr, StartIndex);
  end;

begin
  { Added passed string called LineStr. Example:
    Line[i]: ShowMessage(' Test '); // Comment goes here
    LineStr: ShowMessage('      ');
    These ONLY differ if the user searches for NO COMMENTS.
    The search then finds index matches in LineStr, but highlights them
    using the original Line[i]. }

  Line := FData[LineNo];
  StartIndex := 1;
  GetNextMatch;
  while MatchPos > 0 do
  begin
    StartCol := MatchPos;
    EndCol := MatchPos + Length(Pattern) - 1;
    Assert(StartCol > 0);
    Assert(EndCol > 0);
    if (not WholeWord) or (CheckWholeWord(LineStr, StartCol, EndCol)) then
      if Assigned(FOnFound) then
        FOnFound(LineNo + 1, Offset + StartCol, Offset + EndCol, Line);
    StartIndex := StartCol + 1;
    GetNextMatch;
  end;
end;

procedure TSearcher.SearchLineRegEx(const LineStr: string; LineNo, Offset: Integer);
var
  StartCol: Integer;
  EndCol: Integer;
  Line: TGXUnicodeString;
begin
  Line := FData[LineNo];
  if FRegEx.Exec(LineStr) then repeat
  begin
    //if FRegEx.SubExprMatchCount > 0 then
    //  raise Exception.Create('Subexpression searches are not supported');
    StartCol := FRegEx.MatchPos[0];
    EndCol := StartCol + FRegEx.MatchLen[0] - 1;
    Assert(StartCol > 0);
    Assert(EndCol > 0, 'Invalid regular expression match, try escaping any special characters using ''\''');
    if WholeWord then
      if not CheckWholeWord(LineStr, StartCol, EndCol) then
        Continue;
    if Assigned(FOnFound) then
      FOnFound(LineNo + 1, Offset + StartCol, Offset + EndCol, Line);
  end until not FRegEx.ExecNext;
end;

procedure TSearcher.SetFileName(const Value: string);
begin
  FFileName := Value;
  if not Assigned(FData) then
    FData := TGXUnicodeStringList.Create
  else
    FData.Clear;
  GxOtaLoadFileToUnicodeStrings(FFileName, FData);
end;

end.

