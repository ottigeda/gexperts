unit GX_ClassOptions;

{$I GX_CondDefine.inc}

interface

uses
  Windows,
  SysUtils,
  Classes,
  StdCtrls,
  Controls,
  ComCtrls,
  Forms,
  ExtCtrls,
  Graphics,
  GX_BaseForm;

type
  TClassBrowswerFilters = array[0..8] of Boolean;

type
  TfmClassOptions = class(TfmBaseForm)
    pnlButtons: TPanel;
    pnlContent: TPanel;
    pnlButtonsRight: TPanel;
    pcClassOptions: TPageControl;
    tshGeneric: TTabSheet;
    gbxFonts: TGroupBox;
    lblTreeViewFont: TLabel;
    lblListViewFont: TLabel;
    lblEditorFont: TLabel;
    cbTreeView: TComboBox;
    cbListView: TComboBox;
    cbEditor: TComboBox;
    sTreeView: TEdit;
    sListView: TEdit;
    sEditor: TEdit;
    udTree: TUpDown;
    udList: TUpDown;
    udEditor: TUpDown;
    cbAutoHide: TCheckBox;
    tshFilters: TTabSheet;
    gbxFilters: TGroupBox;
    cbConstants: TCheckBox;
    cbMethods: TCheckBox;
    cbTypes: TCheckBox;
    cbVariables: TCheckBox;
    cbProperties: TCheckBox;
    cbPrivate: TCheckBox;
    cbProtected: TCheckBox;
    cbPublic: TCheckBox;
    cbPublished: TCheckBox;
    gbxDiagram: TGroupBox;
    cbTop: TCheckBox;
    cbStayInPackage: TCheckBox;
    gbxSearch: TGroupBox;
    cbParseRecursing: TCheckBox;
    btnOK: TButton;
    btnCancel: TButton;
    b_TreeFont: TButton;
    b_ListFont: TButton;
    b_EditorFont: TButton;
    procedure b_TreeFontClick(Sender: TObject);
    procedure b_ListFontClick(Sender: TObject);
    procedure b_EditorFontClick(Sender: TObject);
  private
    FTreeFont: TFont;
    FListFont: TFont;
    FEditorFont: TFont;
    procedure EditFont(_Font: TFont; _NameCombo: TComboBox; _SizeUd: TUpDown);
    procedure SetData(_TreeFont, _ListFont, _EditorFont: TFont;
      _AutomaticallyHideBrowser: Boolean;
      _PrimitiveTop, _StayInPackage, _ParseRecursing: Boolean;
      _Filters: TClassBrowswerFilters);
    procedure GetData(_TreeFont, _ListFont, _EditorFont: TFont;
      out _AutomaticallyHideBrowser: Boolean;
      out _PrimitiveTop, _StayInPackage, _ParseRecursing: Boolean;
      out _Filters: TClassBrowswerFilters);
    procedure FontToForm(_Font: TFont; _NameCombo: TComboBox; _SizeUd: TUpDown);
    procedure FormToFont(_Font: TFont; _NameCombo: TComboBox; _SizeUd: TUpDown);
    procedure HandleFontShow(_Sender: TObject);
  public
    class function Execute(_Owner: TWinControl; _TreeFont, _ListFont, _EditorFont: TFont;
      var _AutomaticallyHideBrowser: Boolean;
      var _PrimitiveTop, _StayInPackage, _ParseRecursing: Boolean;
      var _Filters: TClassBrowswerFilters): Boolean;
    constructor Create(_Owner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

uses
  Dialogs,
  GX_dzVclUtils;

constructor TfmClassOptions.Create(_Owner: TComponent);
begin
  inherited;
  FTreeFont := TFont.Create;
  FListFont := TFont.Create;
  FEditorFont := TFont.Create;

  cbTreeView.Items.Assign(Screen.Fonts);
  cbListView.Items.Assign(Screen.Fonts);
  cbEditor.Items.Assign(Screen.Fonts);
end;

destructor TfmClassOptions.Destroy;
begin
  FreeAndNil(FEditorFont);
  FreeAndNil(FListFont);
  FreeAndNil(FTreeFont);
  inherited;
end;

class function TfmClassOptions.Execute(_Owner: TWinControl; _TreeFont, _ListFont, _EditorFont: TFont;
  var _AutomaticallyHideBrowser: Boolean;
  var _PrimitiveTop, _StayInPackage, _ParseRecursing: Boolean;
  var _Filters: TClassBrowswerFilters): Boolean;
var
  frm: TfmClassOptions;
begin
  frm := TfmClassOptions.Create(_Owner);
  try
    frm.SetData(_TreeFont, _ListFont, _EditorFont, _AutomaticallyHideBrowser,
      _PrimitiveTop, _StayInPackage, _ParseRecursing, _Filters);
    Result := (mrOk = frm.ShowModal);
    if Result then
      frm.GetData(_TreeFont, _ListFont, _EditorFont, _AutomaticallyHideBrowser,
        _PrimitiveTop, _StayInPackage, _ParseRecursing, _Filters);
  finally
    FreeAndNil(frm);
  end;
end;

procedure TfmClassOptions.FormToFont(_Font: TFont; _NameCombo: TComboBox; _SizeUd: TUpDown);
var
  FontName: string;
begin
  if TComboBox_GetSelected(_NameCombo, FontName) then
    _Font.Name := FontName;
  _Font.Size := _SizeUd.Position;
end;

procedure TfmClassOptions.FontToForm(_Font: TFont; _NameCombo: TComboBox; _SizeUd: TUpDown);
begin
  TComboBox_Select(_NameCombo, _Font.Name);
  _SizeUd.Position := _Font.Size;
end;

procedure TfmClassOptions.EditFont(_Font: TFont; _NameCombo: TComboBox; _SizeUd: TUpDown);
var
  dlg: TFontDialog;
begin
  FormToFont(_Font, _NameCombo, _SizeUd);
  dlg := TFontDialog.Create(Self);
  try
    dlg.OnShow := HandleFontShow;
    dlg.Font := _Font;
{$IFDEF GX_VER170_up} // Delphi 9/2005 (BDS 2)
    if dlg.Execute(Handle) then begin
{$ELSE}
    if dlg.Execute then begin
{$ENDIF}
      _Font.Assign(dlg.Font);
      FontToForm(_Font, _NameCombo, _SizeUd);
    end;
  finally
    FreeAndNil(dlg);
  end;
end;

procedure TfmClassOptions.HandleFontShow(_Sender: TObject);
begin
  TForm_CenterOn((_Sender as TFontDialog).Handle, Self);
end;

procedure TfmClassOptions.b_EditorFontClick(Sender: TObject);
begin
  EditFont(FEditorFont, cbEditor, udEditor);
end;

procedure TfmClassOptions.b_ListFontClick(Sender: TObject);
begin
  EditFont(FListFont, cbListView, udList);
end;

procedure TfmClassOptions.b_TreeFontClick(Sender: TObject);
begin
  EditFont(FTreeFont, cbTreeView, udTree);
end;

procedure TfmClassOptions.GetData(_TreeFont, _ListFont, _EditorFont: TFont;
  out _AutomaticallyHideBrowser: Boolean;
  out _PrimitiveTop, _StayInPackage, _ParseRecursing: Boolean;
  out _Filters: TClassBrowswerFilters);
var
  i: Integer;
begin
  FormToFont(FTreeFont, cbTreeView, udTree);
  _TreeFont.Assign(FTreeFont);
  FormToFont(FListFont, cbListView, udList);
  _ListFont.Assign(FListFont);
  FormToFont(FEditorFont, cbEditor, udEditor);
  _EditorFont.Assign(FEditorFont);
  _AutomaticallyHideBrowser := cbAutoHide.Checked;

  _PrimitiveTop := cbTop.Checked;
  _StayInPackage := cbStayInPackage.Checked;
  _ParseRecursing := cbParseRecursing.Checked;

  for i := 0 to gbxFilters.ControlCount - 1 do
    if gbxFilters.Controls[i] is TCheckBox then begin
      Tag := TCheckBox(gbxFilters.Controls[i]).Tag;
      _Filters[Tag] := TCheckBox(gbxFilters.Controls[i]).Checked;
    end;
end;

procedure TfmClassOptions.SetData(_TreeFont, _ListFont, _EditorFont: TFont;
  _AutomaticallyHideBrowser: Boolean;
  _PrimitiveTop, _StayInPackage, _ParseRecursing: Boolean;
  _Filters: TClassBrowswerFilters);
var
  i: Integer;
begin
  FTreeFont.Assign(_TreeFont);
  FontToForm(FTreeFont, cbTreeView, udTree);
  FListFont.Assign(_ListFont);
  FontToForm(FListFont, cbListView, udList);
  FEditorFont.Assign(_EditorFont);
  FontToForm(FEditorFont, cbEditor, udEditor);
  cbAutoHide.Checked := _AutomaticallyHideBrowser;

  cbTop.Checked := _PrimitiveTop;
  cbStayInPackage.Checked := _StayInPackage;
  cbParseRecursing.Checked := _ParseRecursing;

  for i := 0 to gbxFilters.ControlCount - 1 do begin
    if gbxFilters.Controls[i] is TCheckBox then begin
      Tag := TCheckBox(gbxFilters.Controls[i]).Tag;
      TCheckBox(gbxFilters.Controls[i]).Checked := _Filters[Tag];
    end;
  end;
end;

end.
