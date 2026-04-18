unit GX_UsesSortGroups;

// Core sorting/grouping logic for uses clause units by namespace prefix.
// No UI or OTA dependencies — pure data structures and formatting.

interface

uses
  Classes, SysUtils;

type
  TGroupSeparator = (gsNone, gsBlankLine, gsComment);

  ///<summary>
  /// Ordered list of namespace prefixes for grouping uses clause units.
  /// Units are assigned to the first group whose prefix matches (longest match wins).
  /// Units without a matching prefix go into a catch-all group (last). </summary>
  TUsesPrefixGroupList = class(TObject)
  private
    FList: TStringList;
    function GetCount: Integer;
    function GetGroup(Index: Integer): string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const _Prefix: string);
    procedure Delete(_Index: Integer);
    procedure Move(_OldIndex, _NewIndex: Integer);
    procedure Clear;
    procedure Assign(_Source: TUsesPrefixGroupList);
    procedure LoadDefaults;
    procedure LoadFromStrings(_Lines: TStrings);
    procedure SaveToStrings(_Lines: TStrings);
    ///<summary>
    /// Returns the group index for a unit name (longest matching prefix wins).
    /// Returns -1 if no prefix matches (catch-all group). </summary>
    function FindGroupIndex(const _UnitName: string): Integer;
    property Count: Integer read GetCount;
    property Groups[Index: Integer]: string read GetGroup;
  end;

  ///<summary>
  /// Static methods for sorting and formatting uses clauses by prefix groups. </summary>
  TUsesGroupSorter = class
  public
    ///<summary>
    /// Sorts units into groups by prefix, each group alphabetically sorted.
    /// Returns one TStringList entry per group containing comma-separated unit names.
    /// Empty groups are omitted. Catch-all group is always last. </summary>
    class function SortAndGroupUnits(_Units: TStrings;
      _PrefixGroups: TUsesPrefixGroupList): TStringList;

    ///<summary>
    /// Formats grouped units into a complete uses clause string.
    /// _GroupedUnits: output from SortAndGroupUnits (one group per line).
    /// _MaxLineLength: wrap within a group when line exceeds this length.
    /// _Separator: how to separate groups.
    /// _Indent: indentation string for unit lines (default: two spaces). </summary>
    class function FormatUsesClause(_GroupedUnits: TStrings;
      _MaxLineLength: Integer; _Separator: TGroupSeparator;
      const _Indent: string = '  '): string;
  end;

implementation

{ TUsesPrefixGroupList }

constructor TUsesPrefixGroupList.Create;
begin
  inherited Create;
  FList := TStringList.Create;
end;

destructor TUsesPrefixGroupList.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

function TUsesPrefixGroupList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TUsesPrefixGroupList.GetGroup(Index: Integer): string;
begin
  Result := FList[Index];
end;

procedure TUsesPrefixGroupList.Add(const _Prefix: string);
begin
  FList.Add(_Prefix);
end;

procedure TUsesPrefixGroupList.Delete(_Index: Integer);
begin
  FList.Delete(_Index);
end;

procedure TUsesPrefixGroupList.Move(_OldIndex, _NewIndex: Integer);
begin
  FList.Move(_OldIndex, _NewIndex);
end;

procedure TUsesPrefixGroupList.Clear;
begin
  FList.Clear;
end;

procedure TUsesPrefixGroupList.Assign(_Source: TUsesPrefixGroupList);
begin
  FList.Assign(_Source.FList);
end;

procedure TUsesPrefixGroupList.LoadDefaults;
begin
  FList.Clear;
  FList.Add('Winapi.');
  FList.Add('System.');
  FList.Add('Data.');
  FList.Add('Vcl.');
  FList.Add('FMX.');
  FList.Add('REST.');
  FList.Add('Web.');
end;

procedure TUsesPrefixGroupList.LoadFromStrings(_Lines: TStrings);
var
  i: Integer;
  s: string;
begin
  FList.Clear;
  for i := 0 to _Lines.Count - 1 do begin
    s := Trim(_Lines[i]);
    if s <> '' then
      FList.Add(s);
  end;
end;

procedure TUsesPrefixGroupList.SaveToStrings(_Lines: TStrings);
var
  i: Integer;
begin
  _Lines.Clear;
  for i := 0 to FList.Count - 1 do
    _Lines.Add(FList[i]);
end;

function TUsesPrefixGroupList.FindGroupIndex(const _UnitName: string): Integer;
var
  i: Integer;
  BestIdx: Integer;
  BestLen: Integer;
  Prefix: string;
begin
  BestIdx := -1;
  BestLen := 0;
  for i := 0 to FList.Count - 1 do begin
    Prefix := FList[i];
    if Length(Prefix) > BestLen then begin
      if SameText(Copy(_UnitName, 1, Length(Prefix)), Prefix) then begin
        BestIdx := i;
        BestLen := Length(Prefix);
      end;
    end;
  end;
  Result := BestIdx;
end;

{ TUsesGroupSorter }

class function TUsesGroupSorter.SortAndGroupUnits(_Units: TStrings;
  _PrefixGroups: TUsesPrefixGroupList): TStringList;
var
  Buckets: array of TStringList;
  CatchAll: TStringList;
  i, GroupIdx, BucketCount: Integer;
  UnitName: string;
  Line: string;
begin
  Result := TStringList.Create;
  try
    BucketCount := _PrefixGroups.Count;
    SetLength(Buckets, BucketCount);
    CatchAll := TStringList.Create;
    try
      CatchAll.Sorted := True;
      CatchAll.CaseSensitive := False;
      for i := 0 to BucketCount - 1 do begin
        Buckets[i] := TStringList.Create;
        Buckets[i].Sorted := True;
        Buckets[i].CaseSensitive := False;
      end;
      try
        // Distribute units into buckets
        for i := 0 to _Units.Count - 1 do begin
          UnitName := Trim(_Units[i]);
          if UnitName = '' then
            Continue;
          GroupIdx := _PrefixGroups.FindGroupIndex(UnitName);
          if GroupIdx >= 0 then
            Buckets[GroupIdx].Add(UnitName)
          else
            CatchAll.Add(UnitName);
        end;

        // Build result: one comma-separated string per non-empty group
        for i := 0 to BucketCount - 1 do begin
          if Buckets[i].Count > 0 then begin
            Line := '';
            for GroupIdx := 0 to Buckets[i].Count - 1 do begin
              if Line <> '' then
                Line := Line + ', ';
              Line := Line + Buckets[i][GroupIdx];
            end;
            Result.Add(Line);
          end;
        end;

        // Catch-all group always last
        if CatchAll.Count > 0 then begin
          Line := '';
          for i := 0 to CatchAll.Count - 1 do begin
            if Line <> '' then
              Line := Line + ', ';
            Line := Line + CatchAll[i];
          end;
          Result.Add(Line);
        end;
      finally
        for i := 0 to BucketCount - 1 do
          FreeAndNil(Buckets[i]);
      end;
    finally
      FreeAndNil(CatchAll);
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

class function TUsesGroupSorter.FormatUsesClause(_GroupedUnits: TStrings;
  _MaxLineLength: Integer; _Separator: TGroupSeparator;
  const _Indent: string = '  '): string;

  function GetGroupLabel(const _GroupLine: string): string;
  var
    DotPos: Integer;
    FirstUnit: string;
  begin
    // Extract prefix from first unit in the group (text before first comma)
    DotPos := Pos(',', _GroupLine);
    if DotPos > 0 then
      FirstUnit := Copy(_GroupLine, 1, DotPos - 1)
    else
      FirstUnit := _GroupLine;
    FirstUnit := Trim(FirstUnit);
    // Find the last dot to extract prefix
    DotPos := Length(FirstUnit);
    while (DotPos > 0) and (FirstUnit[DotPos] <> '.') do
      Dec(DotPos);
    if DotPos > 0 then
      Result := Copy(FirstUnit, 1, DotPos - 1)
    else
      Result := 'Other';
  end;

  procedure SplitCsv(const _Csv: string; _List: TStrings);
  var
    p, Start: Integer;
    s: string;
  begin
    _List.Clear;
    Start := 1;
    for p := 1 to Length(_Csv) do begin
      if _Csv[p] = ',' then begin
        s := Trim(Copy(_Csv, Start, p - Start));
        if s <> '' then
          _List.Add(s);
        Start := p + 1;
      end;
    end;
    s := Trim(Copy(_Csv, Start, MaxInt));
    if s <> '' then
      _List.Add(s);
  end;

  function WrapGroupUnits(const _UnitsCsv: string; _IsLast: Boolean): string;
  var
    Units: TStringList;
    j: Integer;
    CurrentLine: string;
    Candidate: string;
    Suffix: string;
  begin
    Result := '';
    Units := TStringList.Create;
    try
      SplitCsv(_UnitsCsv, Units);

      CurrentLine := _Indent;
      for j := 0 to Units.Count - 1 do begin
        if j < Units.Count - 1 then
          Suffix := ', '
        else if _IsLast then
          Suffix := ';'
        else
          Suffix := ',';

        if CurrentLine = _Indent then begin
          // First unit on this line
          CurrentLine := CurrentLine + Units[j] + Suffix;
        end else begin
          Candidate := CurrentLine + Units[j] + Suffix;
          if Length(Candidate) > _MaxLineLength then begin
            // Wrap: emit current line, start new one
            Result := Result + CurrentLine + #13#10;
            CurrentLine := _Indent + Units[j] + Suffix;
          end else begin
            CurrentLine := Candidate;
          end;
        end;
      end;

      // Emit remaining line
      if CurrentLine <> _Indent then
        Result := Result + CurrentLine;
    finally
      FreeAndNil(Units);
    end;
  end;

var
  i: Integer;
  IsLastGroup: Boolean;
  GroupText: string;
begin
  if _GroupedUnits.Count = 0 then begin
    Result := 'uses' + #13#10 + _Indent + ';';
    Exit;
  end;

  Result := 'uses' + #13#10;

  for i := 0 to _GroupedUnits.Count - 1 do begin
    IsLastGroup := (i = _GroupedUnits.Count - 1);

    // Add separator before group (not before the first one)
    if i > 0 then begin
      case _Separator of
        gsNone: ; // no separator
        gsBlankLine:
          Result := Result + #13#10;
        gsComment:
          Result := Result + _Indent + '{ ' + GetGroupLabel(_GroupedUnits[i]) + ' }' + #13#10;
      end;
    end else begin
      // Comment label for first group too
      if _Separator = gsComment then
        Result := Result + _Indent + '{ ' + GetGroupLabel(_GroupedUnits[i]) + ' }' + #13#10;
    end;

    GroupText := WrapGroupUnits(_GroupedUnits[i], IsLastGroup);
    Result := Result + GroupText;
    if not IsLastGroup then
      Result := Result + #13#10;
  end;
end;

end.
