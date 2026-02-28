unit GX_eRenameIdentifier;

{$I GX_CondDefine.inc}

interface

uses
  Classes, StdCtrls, Controls, Forms, GX_BaseForm;

type
  TfmRenameIdentifier = class(TfmBaseForm)
    lblCurrentName: TLabel;
    lblCurrentNameValue: TLabel;
    lblScope: TLabel;
    lblScopeValue: TLabel;
    lblOccurrences: TLabel;
    lblNewName: TLabel;
    edtNewName: TEdit;
    btnOK: TButton;
    btnCancel: TButton;
  private
    FOldName: string;
    procedure edtNewNameChange(Sender: TObject);
    procedure ValidateInput;
  public
    constructor Create(_Owner: TComponent); override;
    procedure SetInfo(const _OldName, _ScopeDesc: string; _OccurrenceCount: Integer);
    function GetNewName: string;
  end;

implementation

{$R *.dfm}

uses
  SysUtils, Windows, ToolsAPI, Dialogs,
  u_dzVclUtils,
  GX_GenericUtils, GX_OtaUtils, GX_EditReader, GX_EditorExpert,
  GX_RenameIdentifier;

type
  TRenameIdentifierExpert = class(TEditorExpert)
  public
    class function GetName: string; override;
    function GetDisplayName: string; override;
    function GetHelpString: string; override;
    function GetDefaultShortCut: TShortCut; override;
    function HasConfigOptions: Boolean; override;
    procedure Execute(Sender: TObject); override;
  end;

{ TfmRenameIdentifier }

constructor TfmRenameIdentifier.Create(_Owner: TComponent);
begin
  inherited;
  InitDpiScaler;
  edtNewName.OnChange := edtNewNameChange;
end;

procedure TfmRenameIdentifier.SetInfo(const _OldName, _ScopeDesc: string;
  _OccurrenceCount: Integer);
begin
  FOldName := _OldName;
  lblCurrentNameValue.Caption := _OldName;
  lblScopeValue.Caption := _ScopeDesc;
  if _OccurrenceCount = 1 then
    lblOccurrences.Caption := '1 occurrence will be renamed'
  else
    lblOccurrences.Caption := IntToStr(_OccurrenceCount) + ' occurrences will be renamed';
  edtNewName.Text := _OldName;
  edtNewName.SelectAll;
  ValidateInput;
end;

function TfmRenameIdentifier.GetNewName: string;
begin
  Result := Trim(edtNewName.Text);
end;

procedure TfmRenameIdentifier.edtNewNameChange(Sender: TObject);
begin
  ValidateInput;
end;

procedure TfmRenameIdentifier.ValidateInput;
var
  NewName: string;
  I: Integer;
  Valid: Boolean;
begin
  NewName := Trim(edtNewName.Text);
  Valid := False;

  if (NewName <> '') and (not SameText(NewName, FOldName) or (NewName <> FOldName)) then begin
    // Check valid Pascal identifier
    if (NewName[1] = '_') or ((NewName[1] >= 'A') and (NewName[1] <= 'Z'))
      or ((NewName[1] >= 'a') and (NewName[1] <= 'z')) then begin
      Valid := True;
      for I := 2 to Length(NewName) do begin
        if not IsCharIdentifier(NewName[I]) then begin
          Valid := False;
          Break;
        end;
      end;
    end;
  end;

  btnOK.Enabled := Valid;
end;

{ TRenameIdentifierExpert }

class function TRenameIdentifierExpert.GetName: string;
begin
  Result := 'RenameIdentifier';
end;

function TRenameIdentifierExpert.GetDisplayName: string;
begin
  Result := 'Rename Identifier';
end;

function TRenameIdentifierExpert.GetHelpString: string;
begin
  Result :=
    '  This expert renames the identifier at the cursor position ' +
    'throughout the current unit, respecting scope.' + sLineBreak +
    '  It detects local variables, parameters, class members, and unit-level ' +
    'identifiers, and only renames the specific identifier in the correct scope.' + sLineBreak +
    '  All occurrences get the exact casing you type. ' +
    'The change is undoable with a single Ctrl+Z.';
end;

function TRenameIdentifierExpert.GetDefaultShortCut: TShortCut;
begin
  Result := scCtrl + scAlt + Ord('R');
end;

function TRenameIdentifierExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

procedure TRenameIdentifierExpert.Execute(Sender: TObject);
var
  SourceEditor: IOTASourceEditor;
  EditRead: TEditReader;
  Src: string;
  CharPos: TOTACharPos;
  CursorCharIdx: Integer;
  OldName, ScopeDesc, ErrorMsg: string;
  RenamedSource: string;
  OccurrenceCount: Integer;
  Dlg: TfmRenameIdentifier;
  NewName: string;
begin
  if not Ota.TryGetCurrentSourceEditor(SourceEditor) then
    Exit;

  EditRead := TEditReader.Create(SourceEditor.FileName);
  try
    Src := EditRead.GetText;
    CharPos := EditRead.GetCurrentCharPos;
  finally
    FreeAndNil(EditRead);
  end;

  Src := AdjustLineBreaks(Src, tlbsCRLF);
  CursorCharIdx := LinePosToCharPos(Point(CharPos.CharIndex + 1, CharPos.Line), Src);

  // Preview mode: get identifier info without renaming
  if not RenameIdentifierInSource(Src, CursorCharIdx, '',
    RenamedSource, OccurrenceCount, OldName, ScopeDesc, ErrorMsg) then begin
    MessageBeep($FFFFFFFF);
    Exit;
  end;

  Dlg := TfmRenameIdentifier.Create(nil);
  try
    SetFormIcon(Dlg);
    Dlg.SetInfo(OldName, ScopeDesc, OccurrenceCount);
    if Dlg.ShowModal <> mrOk then
      Exit;
    NewName := Dlg.GetNewName;
  finally
    FreeAndNil(Dlg);
  end;

  // Perform the rename
  if not RenameIdentifierInSource(Src, CursorCharIdx, NewName,
    RenamedSource, OccurrenceCount, OldName, ScopeDesc, ErrorMsg) then begin
    ShowError(ErrorMsg);
    Exit;
  end;

  Ota.ReplaceEditorText(SourceEditor, RenamedSource);
  IncCallCount;
end;

initialization
  RegisterEditorExpert(TRenameIdentifierExpert);

end.
