unit GX_GrepOptions;

interface

uses
  SysUtils,
  Classes,
  Controls,
  StdCtrls,
  Forms,
  GX_BaseForm,
  Menus;

type
  TfmGrepOptions = class(TfmBaseForm)
    btnOK: TButton;
    btnCancel: TButton;
    chkGrepUseCurrentIdent: TCheckBox;
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
  public
    class function Execute(var _UseCurrentIdent: Boolean; var _Editor, _Params: string): Boolean;
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  u_dzVclUtils,
  GX_GenericUtils;

{ TfmGrepOptions }

class function TfmGrepOptions.Execute(var _UseCurrentIdent: Boolean;
  var _Editor, _Params: string): Boolean;
var
  Dlg: TfmGrepOptions;
begin
  Dlg := TfmGrepOptions.Create(nil);
  try
    Dlg.chkGrepUseCurrentIdent.Checked := _UseCurrentIdent;
    Dlg.ed_ExternalEditor.Text := _Editor;
    if _Params = '' then
      Dlg.ed_Parameters.Text := '{FILE}'
    else
      Dlg.ed_Parameters.Text := _Params;
    Result := (Dlg.ShowModal = mrOk);
    if Result then begin
      _UseCurrentIdent := Dlg.chkGrepUseCurrentIdent.Checked;
      _Editor := Dlg.ed_ExternalEditor.Text;
      _Params := Dlg.ed_Parameters.Text;
    end;
  finally
    FreeAndNil(Dlg);
  end;
end;

procedure TfmGrepOptions.b_SelectClick(Sender: TObject);
var
  fn: string;
begin
  fn := ed_ExternalEditor.Text;
  if ShowOpenDialog('Select External Editor', '*.exe', fn) then
    ed_ExternalEditor.Text := fn;
end;

constructor TfmGrepOptions.Create(_Owner: TComponent);
begin
  inherited;
  TWinControl_ActivateDropFiles(Self, HandleDropFiles);

  TButton_AddDropdownMenu(b_Parameters, pm_Parameters);
end;

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
