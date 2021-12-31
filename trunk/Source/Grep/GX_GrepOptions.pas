unit GX_GrepOptions;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils,
  Classes,
  Controls,
  StdCtrls,
  Forms,
  Menus,
  ActnList,
  Actions,
  GX_BaseForm,
  GX_Experts;

type
  TfmGrepOptions = class(TfmBaseForm)
    btnOK: TButton;
    btnCancel: TButton;
    chkUseCurrentIdent: TCheckBox;
    ed_ExternalEditor: TEdit;
    l_ExternalEditor: TLabel;
    l_Parameters: TLabel;
    b_Select: TButton;
    ed_Parameters: TEdit;
    b_Parameters: TButton;
    pm_Parameters: TPopupMenu;
    mi_File: TMenuItem;
    mi_Line: TMenuItem;
    mi_Column: TMenuItem;
    procedure b_SelectClick(Sender: TObject);
    procedure mi_FileClick(Sender: TObject);
    procedure mi_LineClick(Sender: TObject);
    procedure mi_ColumnClick(Sender: TObject);
  private
    procedure HandleDropFiles(_Sender: TObject; _Files: TStrings);
  protected
{$IFDEF IDE_IS_HIDPI_AWARE}
    procedure ArrangeControls; override;
{$ENDIF}
  public
    class function Execute(_Owner: TWinControl;
      var _UseCurrentIdent: Boolean; var _Editor, _Params: string): Boolean;
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  ComCtrls, Types,
  u_dzVclUtils,
  GX_GenericUtils, GX_IdeUtils, GX_GExperts, GX_ActionBroker, GX_OtaUtils;

{ TfmGrepOptions }

class function TfmGrepOptions.Execute(_Owner: TWinControl;
  var _UseCurrentIdent: Boolean; var _Editor, _Params: string): Boolean;
var
  frm: TfmGrepOptions;
begin
  frm := TfmGrepOptions.Create(_Owner);
  try
    frm.chkUseCurrentIdent.Checked := _UseCurrentIdent;
    frm.ed_ExternalEditor.Text := _Editor;
    if _Params = '' then
      frm.ed_Parameters.Text := '{FILE}'
    else
      frm.ed_Parameters.Text := _Params;
    Result := (frm.ShowModal = mrOk);
    if Result then begin
      _UseCurrentIdent := frm.chkUseCurrentIdent.Checked;
      _Editor := frm.ed_ExternalEditor.Text;
      _Params := frm.ed_Parameters.Text;
    end;
  finally
    FreeAndNil(frm);
  end;
end;

procedure TfmGrepOptions.b_SelectClick(Sender: TObject);
var
  fn: string;
begin
  fn := ed_ExternalEditor.Text;
  if ShowOpenDialog('Select External Editor', 'exe', fn) then
    ed_ExternalEditor.Text := fn;
end;

constructor TfmGrepOptions.Create(_Owner: TComponent);
begin
  inherited;

  TWinControl_ActivateDropFiles(Self, HandleDropFiles);

  TButton_AddDropdownMenu(b_Parameters, pm_Parameters);

  InitDpiScaler;
end;

{$IFDEF IDE_IS_HIDPI_AWARE}
procedure TfmGrepOptions.ArrangeControls;
var
  t: Integer;
begin
  t := TEdit_AlignBelowLabel(ed_ExternalEditor, l_ExternalEditor);
  TButton_AlignVerticallyTo(b_Select, ed_ExternalEditor);
  l_Parameters.Top := t + 8;
  t := TEdit_AlignBelowLabel(ed_Parameters, l_Parameters);
  Inc(t, 8);
  btnOK.Top := t;
  btnCancel.Top := t;
  b_Parameters.Top := t;
  Self.ClientHeight := t + b_Parameters.Height + 8;
end;
{$ENDIF}

procedure TfmGrepOptions.HandleDropFiles(_Sender: TObject; _Files: TStrings);
begin
  if _Files.Count > 0 then
    ed_ExternalEditor.Text := _Files[0];
end;

procedure TfmGrepOptions.mi_ColumnClick(Sender: TObject);
begin
  ed_Parameters.Text := ed_Parameters.Text + '{COLUMN}';
end;

procedure TfmGrepOptions.mi_FileClick(Sender: TObject);
begin
  ed_Parameters.Text := ed_Parameters.Text + '{FILE}';
end;

procedure TfmGrepOptions.mi_LineClick(Sender: TObject);
begin
  ed_Parameters.Text := ed_Parameters.Text + '{LINE}';
end;

end.
