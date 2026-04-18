unit GX_UsesSortGroupsTest;

interface

uses
  TestFramework,
  Classes,
  GX_UsesSortGroups;

type
  TTestPrefixGroupList = class(TTestCase)
  private
    FGroups: TUsesPrefixGroupList;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // FindGroupIndex
    procedure TestFindGroupIndex_SimpleMatch;
    procedure TestFindGroupIndex_LongestPrefixWins;
    procedure TestFindGroupIndex_CaseInsensitive;
    procedure TestFindGroupIndex_NoMatch;
    procedure TestFindGroupIndex_EmptyGroups;
    // LoadDefaults
    procedure TestLoadDefaults;
    // LoadFromStrings / SaveToStrings
    procedure TestLoadSaveRoundTrip;
    procedure TestLoadFromStrings_SkipsEmpty;
    // Assign
    procedure TestAssign;
    // Add / Delete / Move
    procedure TestAddDelete;
    procedure TestMove;
  end;

  TTestSortAndGroupUnits = class(TTestCase)
  private
    FGroups: TUsesPrefixGroupList;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestBasicGrouping;
    procedure TestAlphabeticalWithinGroup;
    procedure TestCatchAllLast;
    procedure TestEmptyInput;
    procedure TestAllCatchAll;
    procedure TestAllOneGroup;
    procedure TestEmptyGroupsOmitted;
    procedure TestLongestPrefixMatch;
    procedure TestTrimsWhitespace;
    procedure TestDuplicateUnits;
  end;

  TTestFormatUsesClause = class(TTestCase)
  private
    FGroups: TUsesPrefixGroupList;
    function SortAndFormat(_Units: TStrings; _MaxLen: Integer;
      _Sep: TGroupSeparator; const _Indent: string = '  '): string;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // Separator modes
    procedure TestNoSeparator;
    procedure TestBlankLineSeparator;
    procedure TestCommentSeparator;
    // Line wrapping
    procedure TestWrapAtMargin;
    procedure TestNoWrapWhenShort;
    // Edge cases
    procedure TestEmptyInput;
    procedure TestSingleUnit;
    procedure TestSingleGroup;
    // Custom indent
    procedure TestCustomIndent;
    // Comment labels
    procedure TestCommentLabelForCatchAll;
  end;

implementation

uses
  SysUtils;

{ TTestPrefixGroupList }

procedure TTestPrefixGroupList.SetUp;
begin
  FGroups := TUsesPrefixGroupList.Create;
end;

procedure TTestPrefixGroupList.TearDown;
begin
  FreeAndNil(FGroups);
end;

procedure TTestPrefixGroupList.TestFindGroupIndex_SimpleMatch;
begin
  FGroups.Add('Vcl.');
  FGroups.Add('System.');
  CheckEquals(0, FGroups.FindGroupIndex('Vcl.Forms'));
  CheckEquals(1, FGroups.FindGroupIndex('System.SysUtils'));
end;

procedure TTestPrefixGroupList.TestFindGroupIndex_LongestPrefixWins;
begin
  FGroups.Add('System.');
  FGroups.Add('System.Win.');
  // System.Win. is longer and should win even though System. is listed first
  CheckEquals(1, FGroups.FindGroupIndex('System.Win.Registry'));
  // System.SysUtils should match System. (not System.Win.)
  CheckEquals(0, FGroups.FindGroupIndex('System.SysUtils'));
end;

procedure TTestPrefixGroupList.TestFindGroupIndex_CaseInsensitive;
begin
  FGroups.Add('Vcl.');
  CheckEquals(0, FGroups.FindGroupIndex('vcl.Forms'));
  CheckEquals(0, FGroups.FindGroupIndex('VCL.Controls'));
end;

procedure TTestPrefixGroupList.TestFindGroupIndex_NoMatch;
begin
  FGroups.Add('Vcl.');
  CheckEquals(-1, FGroups.FindGroupIndex('MyUnit'));
  CheckEquals(-1, FGroups.FindGroupIndex('SysUtils'));
end;

procedure TTestPrefixGroupList.TestFindGroupIndex_EmptyGroups;
begin
  CheckEquals(-1, FGroups.FindGroupIndex('Anything'));
end;

procedure TTestPrefixGroupList.TestLoadDefaults;
begin
  FGroups.LoadDefaults;
  CheckEquals(7, FGroups.Count);
  CheckEquals('Winapi.', FGroups.Groups[0]);
  CheckEquals('System.', FGroups.Groups[1]);
  CheckEquals('Data.', FGroups.Groups[2]);
  CheckEquals('Vcl.', FGroups.Groups[3]);
  CheckEquals('FMX.', FGroups.Groups[4]);
  CheckEquals('REST.', FGroups.Groups[5]);
  CheckEquals('Web.', FGroups.Groups[6]);
end;

procedure TTestPrefixGroupList.TestLoadSaveRoundTrip;
var
  Lines: TStringList;
  Loaded: TUsesPrefixGroupList;
begin
  FGroups.Add('Alpha.');
  FGroups.Add('Beta.');
  FGroups.Add('Gamma.');

  Lines := TStringList.Create;
  try
    FGroups.SaveToStrings(Lines);
    CheckEquals(3, Lines.Count);
    CheckEquals('Alpha.', Lines[0]);

    Loaded := TUsesPrefixGroupList.Create;
    try
      Loaded.LoadFromStrings(Lines);
      CheckEquals(3, Loaded.Count);
      CheckEquals('Alpha.', Loaded.Groups[0]);
      CheckEquals('Beta.', Loaded.Groups[1]);
      CheckEquals('Gamma.', Loaded.Groups[2]);
    finally
      FreeAndNil(Loaded);
    end;
  finally
    FreeAndNil(Lines);
  end;
end;

procedure TTestPrefixGroupList.TestLoadFromStrings_SkipsEmpty;
var
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.Add('Alpha.');
    Lines.Add('');
    Lines.Add('  ');
    Lines.Add('Beta.');
    FGroups.LoadFromStrings(Lines);
    CheckEquals(2, FGroups.Count);
    CheckEquals('Alpha.', FGroups.Groups[0]);
    CheckEquals('Beta.', FGroups.Groups[1]);
  finally
    FreeAndNil(Lines);
  end;
end;

procedure TTestPrefixGroupList.TestAssign;
var
  Other: TUsesPrefixGroupList;
begin
  FGroups.Add('A.');
  FGroups.Add('B.');
  Other := TUsesPrefixGroupList.Create;
  try
    Other.Assign(FGroups);
    CheckEquals(2, Other.Count);
    CheckEquals('A.', Other.Groups[0]);
    CheckEquals('B.', Other.Groups[1]);
  finally
    FreeAndNil(Other);
  end;
end;

procedure TTestPrefixGroupList.TestAddDelete;
begin
  FGroups.Add('A.');
  FGroups.Add('B.');
  FGroups.Add('C.');
  CheckEquals(3, FGroups.Count);
  FGroups.Delete(1);
  CheckEquals(2, FGroups.Count);
  CheckEquals('A.', FGroups.Groups[0]);
  CheckEquals('C.', FGroups.Groups[1]);
end;

procedure TTestPrefixGroupList.TestMove;
begin
  FGroups.Add('A.');
  FGroups.Add('B.');
  FGroups.Add('C.');
  FGroups.Move(2, 0);
  CheckEquals('C.', FGroups.Groups[0]);
  CheckEquals('A.', FGroups.Groups[1]);
  CheckEquals('B.', FGroups.Groups[2]);
end;

{ TTestSortAndGroupUnits }

procedure TTestSortAndGroupUnits.SetUp;
begin
  FGroups := TUsesPrefixGroupList.Create;
  FGroups.Add('Winapi.');
  FGroups.Add('System.');
  FGroups.Add('Vcl.');
end;

procedure TTestSortAndGroupUnits.TearDown;
begin
  FreeAndNil(FGroups);
end;

procedure TTestSortAndGroupUnits.TestBasicGrouping;
var
  Units, Grouped: TStringList;
begin
  Units := TStringList.Create;
  try
    Units.Add('Vcl.Forms');
    Units.Add('Winapi.Windows');
    Units.Add('System.SysUtils');
    Grouped := TUsesGroupSorter.SortAndGroupUnits(Units, FGroups);
    try
      CheckEquals(3, Grouped.Count);
      // Groups ordered by prefix list order: Winapi, System, Vcl
      CheckEquals('Winapi.Windows', Grouped[0]);
      CheckEquals('System.SysUtils', Grouped[1]);
      CheckEquals('Vcl.Forms', Grouped[2]);
    finally
      FreeAndNil(Grouped);
    end;
  finally
    FreeAndNil(Units);
  end;
end;

procedure TTestSortAndGroupUnits.TestAlphabeticalWithinGroup;
var
  Units, Grouped: TStringList;
begin
  Units := TStringList.Create;
  try
    Units.Add('Vcl.Forms');
    Units.Add('Vcl.Controls');
    Units.Add('Vcl.ActnList');
    Grouped := TUsesGroupSorter.SortAndGroupUnits(Units, FGroups);
    try
      CheckEquals(1, Grouped.Count);
      CheckEquals('Vcl.ActnList, Vcl.Controls, Vcl.Forms', Grouped[0]);
    finally
      FreeAndNil(Grouped);
    end;
  finally
    FreeAndNil(Units);
  end;
end;

procedure TTestSortAndGroupUnits.TestCatchAllLast;
var
  Units, Grouped: TStringList;
begin
  Units := TStringList.Create;
  try
    Units.Add('MyUnit');
    Units.Add('Vcl.Forms');
    Units.Add('OtherUnit');
    Grouped := TUsesGroupSorter.SortAndGroupUnits(Units, FGroups);
    try
      CheckEquals(2, Grouped.Count);
      // Vcl group first, catch-all last
      CheckEquals('Vcl.Forms', Grouped[0]);
      CheckEquals('MyUnit, OtherUnit', Grouped[1]);
    finally
      FreeAndNil(Grouped);
    end;
  finally
    FreeAndNil(Units);
  end;
end;

procedure TTestSortAndGroupUnits.TestEmptyInput;
var
  Units, Grouped: TStringList;
begin
  Units := TStringList.Create;
  try
    Grouped := TUsesGroupSorter.SortAndGroupUnits(Units, FGroups);
    try
      CheckEquals(0, Grouped.Count);
    finally
      FreeAndNil(Grouped);
    end;
  finally
    FreeAndNil(Units);
  end;
end;

procedure TTestSortAndGroupUnits.TestAllCatchAll;
var
  Units, Grouped: TStringList;
begin
  Units := TStringList.Create;
  try
    Units.Add('MyUnit');
    Units.Add('AnotherUnit');
    Grouped := TUsesGroupSorter.SortAndGroupUnits(Units, FGroups);
    try
      CheckEquals(1, Grouped.Count);
      CheckEquals('AnotherUnit, MyUnit', Grouped[0]);
    finally
      FreeAndNil(Grouped);
    end;
  finally
    FreeAndNil(Units);
  end;
end;

procedure TTestSortAndGroupUnits.TestAllOneGroup;
var
  Units, Grouped: TStringList;
begin
  Units := TStringList.Create;
  try
    Units.Add('Vcl.Forms');
    Units.Add('Vcl.Controls');
    Grouped := TUsesGroupSorter.SortAndGroupUnits(Units, FGroups);
    try
      CheckEquals(1, Grouped.Count);
    finally
      FreeAndNil(Grouped);
    end;
  finally
    FreeAndNil(Units);
  end;
end;

procedure TTestSortAndGroupUnits.TestEmptyGroupsOmitted;
var
  Units, Grouped: TStringList;
begin
  Units := TStringList.Create;
  try
    // Only Vcl units — Winapi and System groups should be omitted
    Units.Add('Vcl.Forms');
    Grouped := TUsesGroupSorter.SortAndGroupUnits(Units, FGroups);
    try
      CheckEquals(1, Grouped.Count);
      CheckEquals('Vcl.Forms', Grouped[0]);
    finally
      FreeAndNil(Grouped);
    end;
  finally
    FreeAndNil(Units);
  end;
end;

procedure TTestSortAndGroupUnits.TestLongestPrefixMatch;
var
  Units, Grouped: TStringList;
  Groups: TUsesPrefixGroupList;
begin
  Groups := TUsesPrefixGroupList.Create;
  try
    Groups.Add('System.');
    Groups.Add('System.Win.');
    Units := TStringList.Create;
    try
      Units.Add('System.SysUtils');
      Units.Add('System.Win.Registry');
      Grouped := TUsesGroupSorter.SortAndGroupUnits(Units, Groups);
      try
        CheckEquals(2, Grouped.Count);
        CheckEquals('System.SysUtils', Grouped[0]);
        CheckEquals('System.Win.Registry', Grouped[1]);
      finally
        FreeAndNil(Grouped);
      end;
    finally
      FreeAndNil(Units);
    end;
  finally
    FreeAndNil(Groups);
  end;
end;

procedure TTestSortAndGroupUnits.TestTrimsWhitespace;
var
  Units, Grouped: TStringList;
begin
  Units := TStringList.Create;
  try
    Units.Add('  Vcl.Forms  ');
    Units.Add('');
    Units.Add('  ');
    Units.Add('Vcl.Controls');
    Grouped := TUsesGroupSorter.SortAndGroupUnits(Units, FGroups);
    try
      CheckEquals(1, Grouped.Count);
      CheckEquals('Vcl.Controls, Vcl.Forms', Grouped[0]);
    finally
      FreeAndNil(Grouped);
    end;
  finally
    FreeAndNil(Units);
  end;
end;

procedure TTestSortAndGroupUnits.TestDuplicateUnits;
var
  Units, Grouped: TStringList;
begin
  Units := TStringList.Create;
  try
    Units.Add('Vcl.Forms');
    Units.Add('Vcl.Forms');
    Grouped := TUsesGroupSorter.SortAndGroupUnits(Units, FGroups);
    try
      CheckEquals(1, Grouped.Count);
      // Sorted TStringList deduplicates — correct for uses clauses
      CheckEquals('Vcl.Forms', Grouped[0]);
    finally
      FreeAndNil(Grouped);
    end;
  finally
    FreeAndNil(Units);
  end;
end;

{ TTestFormatUsesClause }

procedure TTestFormatUsesClause.SetUp;
begin
  FGroups := TUsesPrefixGroupList.Create;
  FGroups.Add('Winapi.');
  FGroups.Add('System.');
  FGroups.Add('Vcl.');
end;

procedure TTestFormatUsesClause.TearDown;
begin
  FreeAndNil(FGroups);
end;

function TTestFormatUsesClause.SortAndFormat(_Units: TStrings; _MaxLen: Integer;
  _Sep: TGroupSeparator; const _Indent: string = '  '): string;
var
  Grouped: TStringList;
begin
  Grouped := TUsesGroupSorter.SortAndGroupUnits(_Units, FGroups);
  try
    Result := TUsesGroupSorter.FormatUsesClause(Grouped, _MaxLen, _Sep, _Indent);
  finally
    FreeAndNil(Grouped);
  end;
end;

procedure TTestFormatUsesClause.TestNoSeparator;
var
  Units: TStringList;
  S: string;
begin
  Units := TStringList.Create;
  try
    Units.Add('Winapi.Windows');
    Units.Add('Winapi.Messages');
    Units.Add('System.SysUtils');
    Units.Add('System.Classes');
    Units.Add('Vcl.Controls');
    Units.Add('Vcl.Forms');
    S := SortAndFormat(Units, 80, gsNone);
    CheckEquals(
      'uses'#13#10 +
      '  Winapi.Messages, Winapi.Windows,'#13#10 +
      '  System.Classes, System.SysUtils,'#13#10 +
      '  Vcl.Controls, Vcl.Forms;',
      S);
  finally
    FreeAndNil(Units);
  end;
end;

procedure TTestFormatUsesClause.TestBlankLineSeparator;
var
  Units: TStringList;
  S: string;
begin
  Units := TStringList.Create;
  try
    Units.Add('Winapi.Windows');
    Units.Add('System.SysUtils');
    Units.Add('Vcl.Forms');
    S := SortAndFormat(Units, 80, gsBlankLine);
    CheckEquals(
      'uses'#13#10 +
      '  Winapi.Windows,'#13#10 +
      #13#10 +
      '  System.SysUtils,'#13#10 +
      #13#10 +
      '  Vcl.Forms;',
      S);
  finally
    FreeAndNil(Units);
  end;
end;

procedure TTestFormatUsesClause.TestCommentSeparator;
var
  Units: TStringList;
  S: string;
begin
  Units := TStringList.Create;
  try
    Units.Add('Winapi.Windows');
    Units.Add('System.SysUtils');
    Units.Add('Vcl.Forms');
    S := SortAndFormat(Units, 80, gsComment);
    CheckEquals(
      'uses'#13#10 +
      '  { Winapi }' + #13#10 +
      '  Winapi.Windows,'#13#10 +
      '  { System }' + #13#10 +
      '  System.SysUtils,'#13#10 +
      '  { Vcl }' + #13#10 +
      '  Vcl.Forms;',
      S);
  finally
    FreeAndNil(Units);
  end;
end;

procedure TTestFormatUsesClause.TestWrapAtMargin;
var
  Units: TStringList;
  S: string;
begin
  Units := TStringList.Create;
  try
    Units.Add('Vcl.Controls');
    Units.Add('Vcl.Forms');
    Units.Add('Vcl.Dialogs');
    Units.Add('Vcl.StdCtrls');
    // With a narrow margin, units should wrap
    S := SortAndFormat(Units, 40, gsNone);
    // Line "  Vcl.Controls, Vcl.Dialogs, " would be 30 chars
    // Adding "Vcl.Forms, " makes it 41 > 40, so it wraps
    Check(Pos(#13#10'  Vcl.', S) > 0, 'Expected line wrap within group');
  finally
    FreeAndNil(Units);
  end;
end;

procedure TTestFormatUsesClause.TestNoWrapWhenShort;
var
  Units: TStringList;
  S: string;
  LineCount: Integer;
  i: Integer;
begin
  Units := TStringList.Create;
  try
    Units.Add('Vcl.Forms');
    Units.Add('Vcl.Controls');
    S := SortAndFormat(Units, 200, gsNone);
    // With a wide margin, everything fits on one line (plus the 'uses' line)
    LineCount := 1;
    for i := 1 to Length(S) - 1 do
      if (S[i] = #13) and (S[i + 1] = #10) then
        Inc(LineCount);
    CheckEquals(2, LineCount, 'Expected exactly 2 lines: uses + units');
  finally
    FreeAndNil(Units);
  end;
end;

procedure TTestFormatUsesClause.TestEmptyInput;
var
  Units: TStringList;
  S: string;
begin
  Units := TStringList.Create;
  try
    S := SortAndFormat(Units, 80, gsNone);
    CheckEquals('uses'#13#10'  ;', S);
  finally
    FreeAndNil(Units);
  end;
end;

procedure TTestFormatUsesClause.TestSingleUnit;
var
  Units: TStringList;
  S: string;
begin
  Units := TStringList.Create;
  try
    Units.Add('Vcl.Forms');
    S := SortAndFormat(Units, 80, gsNone);
    CheckEquals('uses'#13#10'  Vcl.Forms;', S);
  finally
    FreeAndNil(Units);
  end;
end;

procedure TTestFormatUsesClause.TestSingleGroup;
var
  Units: TStringList;
  S: string;
begin
  Units := TStringList.Create;
  try
    Units.Add('Vcl.Forms');
    Units.Add('Vcl.Controls');
    S := SortAndFormat(Units, 80, gsNone);
    CheckEquals('uses'#13#10'  Vcl.Controls, Vcl.Forms;', S);
  finally
    FreeAndNil(Units);
  end;
end;

procedure TTestFormatUsesClause.TestCustomIndent;
var
  Units: TStringList;
  S: string;
begin
  Units := TStringList.Create;
  try
    Units.Add('Vcl.Forms');
    S := SortAndFormat(Units, 80, gsNone, '    ');
    CheckEquals('uses'#13#10'    Vcl.Forms;', S);
  finally
    FreeAndNil(Units);
  end;
end;

procedure TTestFormatUsesClause.TestCommentLabelForCatchAll;
var
  Units: TStringList;
  S: string;
begin
  Units := TStringList.Create;
  try
    Units.Add('MyUnit');
    Units.Add('Vcl.Forms');
    S := SortAndFormat(Units, 80, gsComment);
    // Catch-all group should have label "Other"
    Check(Pos('{ Other }', S) > 0, 'Expected "{ Other }" label for catch-all group');
    Check(Pos('{ Vcl }', S) > 0, 'Expected "{ Vcl }" label for Vcl group');
  finally
    FreeAndNil(Units);
  end;
end;

initialization
  RegisterTest(TTestPrefixGroupList.Suite);
  RegisterTest(TTestSortAndGroupUnits.Suite);
  RegisterTest(TTestFormatUsesClause.Suite);
end.
