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
    lblComponentWarning: TLabel;
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
    procedure SetIsComponent(Value: Boolean);
    function GetNewName: string;
  end;

implementation

{$R *.dfm}

uses
  SysUtils, Windows, Menus, ToolsAPI, Dialogs,
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

procedure TfmRenameIdentifier.SetIsComponent(Value: Boolean);
begin
  lblComponentWarning.Visible := Value;
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
  // No default shortcut: Rename Components (TGX_Expert) owns Shift+F2
  // and forwards to us when the code editor is active.
  // Having two actions with the same shortcut breaks older Delphi versions.
  Result := 0;
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
  Step: string;
  FormEditor: IOTAFormEditor;
  Component: IOTAComponent;
  IsComponent: Boolean;
begin
  Step := 'GetSourceEditor';
  try
    if not Ota.TryGetCurrentSourceEditor(SourceEditor) then
      Exit;

    Step := 'ReadSource';
    EditRead := TEditReader.Create(SourceEditor.FileName);
    try
      Src := EditRead.GetText;
      CharPos := EditRead.GetCurrentCharPos;
    finally
      FreeAndNil(EditRead);
    end;

    Step := 'ConvertCursorPos (Line=' + IntToStr(CharPos.Line)
      + ', Col=' + IntToStr(CharPos.CharIndex) + ')';
    Src := AdjustLineBreaks(Src, tlbsCRLF);
    CursorCharIdx := LinePosToCharPos(Point(CharPos.CharIndex + 1, CharPos.Line), Src);

    Step := 'PreviewRename (CursorCharIdx=' + IntToStr(CursorCharIdx)
      + ', SrcLen=' + IntToStr(Length(Src)) + ')';
    // Preview mode: get identifier info without renaming
    if not RenameIdentifierInSource(Src, CursorCharIdx, '',
      RenamedSource, OccurrenceCount, OldName, ScopeDesc, ErrorMsg) then begin
      MessageBeep($FFFFFFFF);
      Exit;
    end;

    // Check if identifier is a component in the form designer
    Step := 'DetectComponent';
    IsComponent := False;
    FormEditor := GxOtaGetFormEditorFromModule(GxOtaGetCurrentModule);
    if Assigned(FormEditor) then begin
      // Block renaming the form's name or type (e.g. Form1 or TForm1)
      Component := FormEditor.GetRootComponent;
      if Assigned(Component) then begin
        if SameText(OldName, Ota.GetComponentName(Component))
          or SameText(OldName, Component.GetComponentType) then begin
          ShowError('Cannot rename the form type here. Use the Object Inspector to rename the form.');
          Exit;
        end;
      end;
      Component := FormEditor.FindComponent(OldName);
      IsComponent := Assigned(Component);
    end;

    Step := 'ShowDialog';
    Dlg := TfmRenameIdentifier.Create(nil);
    try
      SetFormIcon(Dlg);
      Dlg.SetInfo(OldName, ScopeDesc, OccurrenceCount);
      Dlg.SetIsComponent(IsComponent);
      if Dlg.ShowModal <> mrOk then
        Exit;
      NewName := Dlg.GetNewName;
    finally
      FreeAndNil(Dlg);
    end;

    if IsComponent then begin
      // Component path: rename in DFM first, then update body references
      Step := 'RenameComponent (OldName=' + OldName + ', NewName=' + NewName + ')';
      if not Ota.SetComponentName(Component, NewName) then begin
        ShowError('Failed to rename component in form designer.');
        Exit;
      end;
      // IDE has renamed DFM + field declaration; re-read source
      Step := 'ReReadSource';
      EditRead := TEditReader.Create(SourceEditor.FileName);
      try
        Src := EditRead.GetText;
      finally
        FreeAndNil(EditRead);
      end;
      Src := AdjustLineBreaks(Src, tlbsCRLF);
      // Rename remaining body references
      Step := 'RenameBodyReferences';
      RenamedSource := RenameAllTokenOccurrences(Src, OldName, NewName, OccurrenceCount);
      if OccurrenceCount > 0 then
        Ota.ReplaceEditorText(SourceEditor, RenamedSource);
    end else begin
      // Normal non-component path
      Step := 'PerformRename (OldName=' + OldName + ', NewName=' + NewName + ')';
      if not RenameIdentifierInSource(Src, CursorCharIdx, NewName,
        RenamedSource, OccurrenceCount, OldName, ScopeDesc, ErrorMsg) then begin
        ShowError(ErrorMsg);
        Exit;
      end;
      Step := 'ReplaceEditorText';
      Ota.ReplaceEditorText(SourceEditor, RenamedSource);
    end;
    IncCallCount;
  except
    on E: Exception do
      ShowError('Rename Identifier error at step: ' + Step + sLineBreak
        + E.ClassName + ': ' + E.Message);
  end;
end;

initialization
  RegisterEditorExpert(TRenameIdentifierExpert);

end.
