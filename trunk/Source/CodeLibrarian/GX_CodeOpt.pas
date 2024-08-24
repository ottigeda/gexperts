unit GX_CodeOpt;

interface

uses
  Classes, Controls, Forms, StdCtrls, ExtCtrls, ComCtrls, Graphics, GX_BaseForm;

type
  TCodeLayout = (clSide, clTop);

type
  TfmCodeOptions = class(TfmBaseForm)
    btnOK: TButton;
    btnCancel: TButton;
    pgeCodeOpt: TPageControl;
    tabPaths: TTabSheet;
    tabLayout: TTabSheet;
    tabFonts: TTabSheet;
    lblStoragePath: TLabel;
    edPath: TEdit;
    sbBrowse: TButton;
    rbSide: TRadioButton;
    pnlSideSide: TPanel;
    shpLeft: TShape;
    shpRight: TShape;
    rbTop: TRadioButton;
    pnlTopBottom: TPanel;
    shpTop: TShape;
    shpBottom: TShape;
    lblTreeView: TLabel;
    lblEditor: TLabel;
    fcTreeview: TComboBox;
    fcEditor: TComboBox;
    lblSize: TLabel;
    udTreeview: TUpDown;
    udEditor: TUpDown;
    eTreeview: TEdit;
    eEditor: TEdit;
    procedure sbBrowseClick(Sender: TObject);
    procedure eNumericKeyPress(Sender: TObject; var Key: Char);
  private
    procedure InitializeForm;
    procedure edPathOnFilesDropped(_Sender: TObject; _Files: TStrings);
    procedure SetData(const _StoragePath: string; _Layout: TCodeLayout;
      _TreeFont, _EditFont: TFont);
    procedure GetData(out _StoragePath: string; out _Layout: TCodeLayout;
      _TreeFont, _EditFont: TFont);
  public
    class function Execute(_Owner: TWinControl; var _StoragePath: string; var _Layout: TCodeLayout;
      _TreeFont, _EditFont: TFont): Boolean;
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  SysUtils, GX_GenericUtils, u_dzVclUtils;

class function TfmCodeOptions.Execute(_Owner: TWinControl; var _StoragePath: string;
  var _Layout: TCodeLayout; _TreeFont, _EditFont: TFont): Boolean;
var
  frm: TfmCodeOptions;
begin
  frm := TfmCodeOptions.Create(nil);
  try
    TForm_CenterOn(frm, _Owner);
    frm.SetData(_StoragePath, _Layout, _TreeFont, _EditFont);
    Result :=( frm.ShowModal = mrOk);
    if Result then
      frm.GetData(_StoragePath, _Layout, _TreeFont, _EditFont);
  finally
    FreeAndNil(frm);
  end;
end;

procedure TfmCodeOptions.SetData(const _StoragePath: string; _Layout: TCodeLayout;
  _TreeFont, _EditFont: TFont);
begin
  edPath.Text := _StoragePath;

  if _Layout = clSide then
    rbSide.Checked := True
  else
    rbTop.Checked := True;

  fcTreeview.ItemIndex := fcTreeview.Items.IndexOf(_TreeFont.Name);
  udTreeview.Position := _TreeFont.Size;

  fcEditor.ItemIndex := fcEditor.Items.IndexOf(_EditFont.Name);
  udEditor.Position := _EditFont.Size;
end;

procedure TfmCodeOptions.GetData(out _StoragePath: string; out _Layout: TCodeLayout;
  _TreeFont, _EditFont: TFont);
begin
  _StoragePath := edPath.Text;
  if rbSide.Checked then
    _Layout := clSide
  else
    _Layout := clTop;

  _TreeFont.Name := fcTreeview.Text;
  _TreeFont.Size := Trunc(StrToInt(eTreeview.Text));
  _EditFont.Name := fcEditor.Text;
  _EditFont.Size := Trunc(StrToInt(eEditor.Text));
end;

procedure TfmCodeOptions.sbBrowseClick(Sender: TObject);
var
  Temp: string;
begin
  Temp := edPath.Text;
  if GetDirectory(Temp) then
    edPath.Text := Temp;
end;

procedure TfmCodeOptions.InitializeForm;
begin
  fcTreeview.Items.Assign(Screen.Fonts);
  fcEditor.Items.Assign(Screen.Fonts);
end;

constructor TfmCodeOptions.Create(_Owner: TComponent);
begin
  inherited;

  TControl_SetConstraints(Self, [ccMinWidth, ccMinHeight, ccMaxHeight]);
  TWinControl_ActivateDropFiles(edPath, edPathOnFilesDropped);
  TEdit_ActivateAutoComplete(edPath, [acsFileSystem], [actSuggest]);

  InitDpiScaler;

  InitializeForm;
end;

procedure TfmCodeOptions.eNumericKeyPress(Sender: TObject; var Key: Char);
begin
  if not IsCharNumeric(Key) or IsCharTab(Key) then
    Key := #0;
end;

procedure TfmCodeOptions.edPathOnFilesDropped(_Sender: TObject; _Files: TStrings);
begin
  edPath.Text := _Files[0];
end;

end.
