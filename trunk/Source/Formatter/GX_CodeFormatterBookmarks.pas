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
    FCursorPos: TOTAEditPos;
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
  if not GxOtaTryGetCurrentSourceEditor(_SourceEditor) then
    Exit; //==>
  _EditView := _SourceEditor.GetEditView(0);
  Result := Assigned(_EditView);
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
    Exit; //==>

  FCursorPos := EditView.GetCursorPos;

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
  SavedCursorPos: TOTAEditPos;
  BmPos: TOTACharPos;
  BmEditPos: TOTAEditPos;
  i: Integer;
  NewLineNo: Integer;
begin
  if not GetEditView(SourceEditor, EditView) then
    Exit; //==>

  SavedCursorPos := EditView.GetCursorPos;

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

  if _NewCode.FindNewLineNo(FCursorPos.Line, NewLineNo) then begin
    FCursorPos.Line := NewLineNo;
    EditView.SetCursorPos(FCursorPos);
  end else
    EditView.SetCursorPos(SavedCursorPos);

  EditView.Paint;
  SourceEditor.Show;
  FBookmarks.Clear;
end;

end.
