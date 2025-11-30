// stores the bookmarks while the source file is being reformatted
// Original Author:     Thomas Mueller (http://www.dummzeuch.de)
unit GX_CodeFormatterBookmarks;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils,
  Classes,
  ToolsApi,
  GX_CodeFormatterSourceLineList,
  GX_BookmarkList;

type
  TBookmarkHandler = class
  private
    FCursorPosArr: array of TOTAEditPos;
    FBookmarks: TBookmarkList;
  protected
    function GetEditView(var _SourceEditor: IOTASourceEditor; var _EditView: IOTAEditView): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    /// @param NewCode has a method FindNewLineNo for calculating the new line numbers for the
    ///                bookmarks. </summary>
    procedure RestoreItems(_NewCode: TSourceLineList);
    procedure SaveItems;
  end;

implementation

uses
  GX_GenericUtils,
  GX_OtaUtils;

{ TBookmarkHandler }

constructor TBookmarkHandler.Create;
begin
  inherited Create;
  FBookmarks := TBookmarkList.Create;
end;

destructor TBookmarkHandler.Destroy;
begin
  FBookmarks.Free;
  inherited;
end;

function TBookmarkHandler.GetEditView(var _SourceEditor: IOTASourceEditor; var _EditView: IOTAEditView): Boolean;
begin
  Result := False;
  if not Ota.TryGetCurrentSourceEditor(_SourceEditor) then
    Exit; //==>
  _EditView := _SourceEditor.GetEditView(0);
  Result := Assigned(_EditView);
end;

procedure TBookmarkHandler.SaveItems;
var
  SourceEditor: IOTASourceEditor;
  EditView: IOTAEditView;
  EditViewCount: Integer;
  i: Integer;
  BmPos: TOTACharPos;
begin
  FBookmarks.Clear;
  if not GetEditView(SourceEditor, EditView) then
    Exit; //==>

  EditViewCount := SourceEditor.EditViewCount;
  SetLength(FCursorPosArr,EditViewCount);
  for i := 0 to EditViewCount - 1 do
    FCursorPosArr[i] := SourceEditor.EditViews[i].GetCursorPos;

  for i := 0 to 19 do begin
    BmPos := EditView.BookmarkPos[i];
    if BmPos.Line <> 0 then begin
      FBookmarks.Add(i, BmPos.Line, BmPos.CharIndex);
    end;
  end;
end;

procedure TBookmarkHandler.RestoreItems(_NewCode: TSourceLineList);
var
  SourceEditor: IOTASourceEditor;
  EditView: IOTAEditView;
  CursorPos: TOTAEditPos;
  SavedCursorPosArr: array of TOTAEditPos;
  EditViewCount: Integer;
  BmPos: TOTACharPos;
  BmEditPos: TOTAEditPos;
  i: Integer;
  NewLineNo: Integer;
begin
  if not GetEditView(SourceEditor, EditView) then
    Exit; //==>

  EditViewCount := SourceEditor.EditViewCount;
  SetLength(SavedCursorPosArr, EditViewCount);
  for i := 0 to EditViewCount - 1 do
    SavedCursorPosArr[i] := SourceEditor.EditViews[i].GetCursorPos;

  for i := 0 to FBookmarks.Count - 1 do begin
    BmPos.Line := FBookmarks[i].Line;
    if _NewCode.FindNewLineNo(BmPos.Line, NewLineNo) then begin
      BmPos.Line := NewLineNo;
      BmPos.CharIndex := FBookmarks[i].CharIdx;
      EditView.ConvertPos(False, BmEditPos, BmPos);
      EditView.SetCursorPos(BmEditPos);
      EditView.BookmarkToggle(FBookmarks[i].Number);
    end;
  end;

  EditViewCount := SourceEditor.EditViewCount;
  for i := 0 to Length(FCursorPosArr) - 1 do begin
    if i < EditViewCount then begin
      CursorPos := FCursorPosArr[i];
      if _NewCode.FindNewLineNo(CursorPos.Line, NewLineNo) then begin
        CursorPos.Line := NewLineNo;
        SourceEditor.EditViews[i].SetCursorPos(CursorPos);
      end else begin
        if i < Length(SavedCursorPosArr) then
          SourceEditor.EditViews[i].SetCursorPos(SavedCursorPosArr[i]);
      end;
    end;
  end;

// not sure wether we need this, so I commented it out for the moment
//  EditView.Paint;
//  SourceEditor.Show;
  FBookmarks.Clear;
end;

end.
