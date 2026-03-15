unit GX_eDuplicateLine;

{$I GX_CondDefine.inc}

interface

uses
  Classes,
  GX_EditorExpert;

type
  TDuplicateLineExpert = class(TEditorExpert)
  public
    class function GetName: string; override;
    function GetDisplayName: string; override;
    function GetHelpString: string; override;
    function GetDefaultShortCut: TShortCut; override;
    procedure Execute(Sender: TObject); override;
  end;

implementation

uses
  ToolsAPI,
  GX_OtaUtils;

procedure TDuplicateLineExpert.Execute(Sender: TObject);
var
  EditView: IOTAEditView;
  CurPos, NewPos: TOTAEditPos;
  LineText: string;
begin
  EditView := Ota.GetTopMostEditView;
  if not Assigned(EditView) then
    Exit;

  CurPos   := Ota.GetCurrentEditPos(EditView);
  LineText := Ota.GetEditorLineAsString(EditView, CurPos.Line);
  // GetEditorLineAsString reads to MaxEditorCol, which spans to the next line;
  // strip any trailing line-ending characters the read may have included.
  while (Length(LineText) > 0) and
        ((LineText[Length(LineText)] = #13) or (LineText[Length(LineText)] = #10)) do
    Delete(LineText, Length(LineText), 1);

  // Move to end of current line, then insert newline + duplicate
  NewPos.Line := CurPos.Line;
  NewPos.Col  := Length(LineText) + 1;
  EditView.CursorPos := NewPos;
  Ota.InsertTextIntoEditor(#13#10 + LineText);

  // Move cursor to original column on new line
  NewPos.Line := CurPos.Line + 1;
  NewPos.Col  := CurPos.Col;
  EditView.CursorPos := NewPos;
  EditView.Paint;

  IncCallCount;
end;

class function TDuplicateLineExpert.GetName: string;
begin
  Result := 'DuplicateLine';
end;

function TDuplicateLineExpert.GetDisplayName: string;
begin
  Result := 'Duplicate Line';
end;

function TDuplicateLineExpert.GetHelpString: string;
begin
  Result := 'Duplicates the current line, inserting a copy immediately below '
          + 'and placing the cursor at the same column on the new line.';
end;

function TDuplicateLineExpert.GetDefaultShortCut: TShortCut;
begin
  Result := 0;   // No default - user assigns in GExperts configuration
end;

initialization
  RegisterEditorExpert(TDuplicateLineExpert);
end.
