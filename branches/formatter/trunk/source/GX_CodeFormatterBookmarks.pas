// stores the bookmarks while the source file is being reformatted
// Original Author:     Thomas Mueller (http://www.dummzeuch.de)
unit GX_CodeFormatterBookmarks;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils,
  Classes,
  ToolsApi;

type
  TBookmark = class
  private
    fLine: Integer;
    fNumber: Integer;
  public
    constructor Create(_Number, _Line: Integer);
    property Number: Integer read fNumber;
    property Line: Integer read fLine;
  end;

  TBookmarks = class
  private
    function GetCount: Integer;
    function GetItems(_Idx: Integer): TBookmark;
  protected
    fList: TList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(_Number, _Line: Integer);
    procedure Clear;
    property Items[_Idx: Integer]: TBookmark read GetItems; default;
    property Count: Integer read GetCount;
  end;

  TBookmarkHandler = class
  private
    FBookmarks: TBookmarks;
  protected
    function GetEditView(var ASourceEditor: IOTASourceEditor; var AEditView: IOTAEditView): boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RestoreItems;
    procedure SaveItems;
  end;

implementation

uses
  GX_OtaUtils;

{ TBookmark }

constructor TBookmark.Create(_Number, _Line: Integer);
begin
  inherited Create;
  fNumber := _Number;
  fLine := _Line;
end;

{ TBookmarks }

constructor TBookmarks.Create;
begin
  inherited;
  fList := TList.Create;
end;

destructor TBookmarks.Destroy;
var
  i: Integer;
begin
  if Assigned(fList) then begin
    for i := 0 to fList.Count - 1 do
      TBookmark(fList[i]).Free;
    fList.Free;
    fList := nil;
  end;
  inherited;
end;

procedure TBookmarks.Add(_Number, _Line: Integer);
begin
  fList.Add(TBookmark.Create(_Number, _Line))
end;

function TBookmarks.GetCount: Integer;
begin
  Result := fList.Count;
end;

function TBookmarks.GetItems(_Idx: Integer): TBookmark;
begin
  Result := fList[_Idx];
end;

procedure TBookmarks.Clear;
var
  i: Integer;
begin
  for i := 0 to fList.Count - 1 do
    TBookmark(fList[i]).Free;
  fList.Clear;
end;

{ TBookmarkHandler }

constructor TBookmarkHandler.Create;
begin
  inherited Create;
  FBookmarks := TBookmarks.Create;
end;

destructor TBookmarkHandler.Destroy;
begin
  FBookmarks.Free;
  inherited;
end;

function TBookmarkHandler.GetEditView(var ASourceEditor: IOTASourceEditor; var AEditView: IOTAEditView): boolean;
begin
  Result := False;
  ASourceEditor := GxOtaGetCurrentSourceEditor;
  if not Assigned(ASourceEditor) then
    Exit;
  AEditView := ASourceEditor.GetEditView(0);
  Result := Assigned(AEditView);
end;

procedure TBookmarkHandler.SaveItems;
var
  SourceEditor: IOTASourceEditor;
  EditView: IOTAEditView;
  i: Integer;
  BmPos: TOTACharPos;
begin
  FBookmarks.Clear;
  if not GetEditView(SourceEditor, EditView) then
    Exit;
  for i := 0 to 19 do begin
    BmPos := EditView.BookmarkPos[i];
    if BmPos.Line <> 0 then
      FBookmarks.Add(i, BmPos.Line);
  end;
end;

procedure TBookmarkHandler.RestoreItems;
var
  SourceEditor: IOTASourceEditor;
  EditView: IOTAEditView;
  SaveCursorPos: TOTAEditPos;
  BmEditPos: TOTAEditPos;
  i: Integer;
begin
  if not GetEditView(SourceEditor, EditView) then
    Exit;
  SaveCursorPos := EditView.GetCursorPos;
  try
    for i := 0 to FBookmarks.Count - 1 do begin
      BmEditPos.Line := FBookmarks[i].Line;
      BmEditPos.Col := 1;
      EditView.SetCursorPos(BmEditPos);
      EditView.BookmarkToggle(FBookmarks[i].Number);
    end;
  finally
    EditView.SetCursorPos(SaveCursorPos);
  end;
  EditView.Paint;
  SourceEditor.Show;
  FBookmarks.Clear;
end;

end.

