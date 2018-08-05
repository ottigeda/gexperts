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
    procedure SearchLineRegEx(const LineStr: string; LineNo: Integer);
    procedure SearchLineRaw(const LineStr: string; LineNo: Integer);
    procedure SetFileName(const Value: string);
    function CheckWholeWord(Line: TGXUnicodeString; StartCol, EndCol: Integer): Boolean;
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
    property RegularExpression: Boolean read FRegularExpression write FRegularExpression;
    property WholeWord: Boolean read FWholeWord write FWholeWord;
    property FileName: string read FFileName write SetFileName;
    procedure Execute; overload;
  end;

implementation

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

procedure TSearcher.Execute;
var
  i, index, index2: Integer;
  UnitSectionsToSkip: TUnitSections;
  CodeFragmentsToSkip: TCodeFragments;
  ActiveSection: TUnitSection;
  ActiveCodeFragment: TCodeFragment;
  Line: TGXUnicodeString;
  PrevLine: TGXUnicodeString;
//  s: TGXUnicodeString;
//  Len: Integer;
  IsAForm: Boolean;

  procedure doSearchLine(_Line: TGXUnicodeString; _LineIdx: Integer);
  begin
    if RegularExpression then
      SearchLineRegEx(_Line, _LineIdx)
    else
      SearchLineRaw(_Line, _LineIdx);
  end;

  function _TrySwitchToSection(const aNextSection: TUnitSection): Boolean;
  begin
    if ActiveSection < aNextSection then
      if Length(Line) >= index + Length(CSections[aNextSection]) then
        if StrLIComp(@(Line[index]), CSections[aNextSection], Length(CSections[aNextSection])) = 0 then
          if (aNextSection = usEnd) or (not IsCharIdentifier(Line[index + Length(CSections[aNextSection])])) then
          begin
            ActiveSection := aNextSection;
            Result := True;
            Exit;
          end;

    Result := False;
  end; // _TrySwitchToSection

begin // Execute
  if not Assigned(FData) then
    raise Exception.Create('Data to search not provided');
  if IsEmpty(Pattern) then
    raise Exception.Create('Search pattern is empty');

  if RegularExpression then
  begin
    FRegEx.ModifierI := not CaseSensitive;
    FRegEx.Expression := Pattern;
    FRegEx.Compile;
  end;

  UnitSectionsToSkip := [];
  CodeFragmentsToSkip := [];

  if IsPascalSourceFile then
  begin
    // Determine which sections must be skipped :
    if not SectionInterface then Include(UnitSectionsToSkip, usInterface);
    if not SectionImplementation then Include(UnitSectionsToSkip, usImplementation);
    if not SectionInitialization then Include(UnitSectionsToSkip, usInitialization);
    if not SectionFinalization then Include(UnitSectionsToSkip, usFinalization);

    // Determine which code fragments must be skipped :
    if NoCode then CodeFragmentsToSkip := CodeFragmentsToSkip + CodeFragments;
    if NoStrings then CodeFragmentsToSkip := CodeFragmentsToSkip + StringFragments;
    if NoComments then CodeFragmentsToSkip := CodeFragmentsToSkip + CommentFragments;
  end;

  IsAForm := IsForm(FileName);
  ActiveSection := usStart;
  ActiveCodeFragment := cfCode;
  PrevLine := '';
  Index := 0;
  for i := 0 to FData.Count - 1 do
  begin
    // Read the input line, skip if empty.
    Line := FData[i];
    if Line = EmptyString then begin
      PrevLine := '';
      Continue;
    end;

    if IsPascalSourceFile then begin
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
                  if _TrySwitchToSection(usInterface)
                  or _TrySwitchToSection(usImplementation)
                  or _TrySwitchToSection(usInitialization)
                  or _TrySwitchToSection(usFinalization)
                  or _TrySwitchToSection(usEnd) then
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
    end else if IsAForm then begin
      { todo:
        handle the case where long strings are split into multiple lines by the IDE, e.g.:
        Caption = 'abcde bcdef cdefg defgh efghi fghij ghijk hijkl ijklm jklmn klmno lmnop mnopq'
        split into
        Caption =
          'abcde bcdef cdefg defgh efghi fghij ghijk hijkl ijklm jklmn klmn' +
          'o lmnop mnopq'
        grep won't find 'bsdf' because it was split }

      // unfortunately this is more complex than my first take at it:
//      Len := Length(Line);
//      Assert(Len > 0);
//      s := Trim(Line);
//      if (PrevLine <> '') then begin
//        if (Copy(s, 1, 1) = '''') then begin
//          PrevLine := PrevLine + Copy(s, 2, Length(s) - 1);
//        end;
//      end;
//      if Len > 3 then begin
//        if Copy(Line, Len - 2, 3) = ''' +' then begin
//          if PrevLine = '' then begin
//            PrevLine := Copy(Line, 1, Len - 3);
//            Index := i;
//          end else begin
//            PrevLine := PrevLine + Copy(s, 2, Length(s) - 4);
//          end;
//        end else begin
//          if PrevLine <> '' then
//            doSearchLine(PrevLine, index);
//            PrevLine := '';
//          end;
//      end;
      // so I commented it out again
    end;
    doSearchLine(Line, i);
  end; // for i FData
  if PrevLine <> '' then
    doSearchLine(PrevLine + '''', FData.Count - 1);
end; // Execute

procedure TSearcher.SearchLineRaw(const LineStr: string; LineNo: Integer);
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
        FOnFound(LineNo + 1, StartCol, EndCol, Line);
    StartIndex := StartCol + 1;
    GetNextMatch;
  end;
end;

procedure TSearcher.SearchLineRegEx(const LineStr: string; LineNo: Integer);
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
      FOnFound(LineNo + 1, StartCol, EndCol, Line);
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

