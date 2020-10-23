unit GX_UnitExportListTest;

{$OPTIMIZATION off}

interface

uses
  Classes,
  SysUtils,
  TestFrameWork,
  GX_UnitExportList;

type
  TTestUnitExportList = class(TTestCase)
  private
    FList: TUnitExportlist;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure testAddBlaBlub;
    procedure testSortBlaBlub;
    procedure testDuplicates;
    procedure testSearchAnywhere;
  end;

implementation

{ TTestUnitExportList }

procedure TTestUnitExportList.SetUp;
begin
  inherited;
  FList := TUnitExportlist.Create(10);
end;

procedure TTestUnitExportList.TearDown;
begin
  FreeAndNil(FList);
  inherited;
end;

procedure TTestUnitExportList.testAddBlaBlub;
begin
  CheckEquals(0, FList.Add('Bla', 'blaunit'));
  CheckEquals(1, FList.Add('blUb', 'blubunit'));
  CheckEquals(2, FList.Count);
  CheckEquals('Bla', FList.Items[0].Identifier);
  CheckEquals('blUb', FList.Items[1].Identifier);
  CheckEquals('bla', FList.Items[0].LCIdentifier);
  CheckEquals('blub', FList.Items[1].LCIdentifier);
  CheckEquals('blaunit', FList.Items[0].FileName);
  CheckEquals('blubunit', FList.Items[1].FileName);
end;

procedure TTestUnitExportList.testDuplicates;
var
  found: TList;
begin
  CheckEquals(0, FList.Add('Bla', 'Blaunit'));
  CheckEquals(1, FList.Add('bLa', 'bLaunit'));
  CheckEquals(2, FList.Count);
  CheckEquals('Bla', FList.Items[0].Identifier);
  CheckEquals('bLa', FList.Items[1].Identifier);
  CheckEquals('bla', FList.Items[0].LCIdentifier);
  CheckEquals('bla', FList.Items[1].LCIdentifier);
  CheckEquals('Blaunit', FList.Items[0].FileName);
  CheckEquals('bLaunit', FList.Items[1].FileName);
  FList.Sort;
  CheckEquals('bLa', FList.Items[0].Identifier);
  CheckEquals('Bla', FList.Items[1].Identifier);
  CheckEquals('bla', FList.Items[0].LCIdentifier);
  CheckEquals('bla', FList.Items[1].LCIdentifier);
  CheckEquals('bLaunit', FList.Items[0].FileName);
  CheckEquals('Blaunit', FList.Items[1].FileName);
  found := TList.Create;
  try
    CheckTrue(FList.SearchStart('bla', found));
    CheckEquals(2, found.Count);
    CheckEquals('bla', TUnitExport(found[0]).LCIdentifier);
    CheckEquals('bla', TUnitExport(found[1]).LCIdentifier);
  finally
    FreeAndNil(found);
  end;
end;

procedure TTestUnitExportList.testSearchAnywhere;
var
  found: TList;
begin
  testSortBlaBlub;
  found := TList.Create;
  try
    CheckTrue(FList.SearchAnywhere('l', found));
    CheckEquals(2, found.Count);
    CheckEquals('bla', TUnitExport(found[0]).LCIdentifier);
    CheckEquals('blub', TUnitExport(found[1]).LCIdentifier);
  finally
    FreeAndNil(found);
  end;
end;

procedure TTestUnitExportList.testSortBlaBlub;
var
  found: TList;
begin
  testAddBlaBlub;
  FList.Sort;
  CheckEquals('Bla', FList.Items[0].Identifier);
  CheckEquals('blUb', FList.Items[1].Identifier);
  CheckEquals('bla', FList.Items[0].LCIdentifier);
  CheckEquals('blub', FList.Items[1].LCIdentifier);
  CheckEquals('blaunit', FList.Items[0].FileName);
  CheckEquals('blubunit', FList.Items[1].FileName);

  found := TList.Create;
  try
    CheckTrue(FList.SearchStart('bla', found));
    CheckEquals(1, found.Count);
    CheckEquals('bla', TUnitExport(found[0]).LCIdentifier);

    found.Clear;
    CheckTrue(FList.SearchStart('blub', found));
    CheckEquals(1, found.Count);
    CheckEquals('blub', TUnitExport(found[0]).LCIdentifier);
  finally

    FreeAndNil(found);
  end;
end;

initialization
  RegisterTest(TTestUnitExportList.Suite);
end.
