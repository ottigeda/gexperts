unit GX_CodeOpt;

{$I GX_CondDefine.inc}

interface

uses
  Classes, Controls, Forms, StdCtrls, ExtCtrls, ComCtrls, Graphics, GX_BaseForm;

type
  TCodeLayout = (clSide, clTop, clUnknown);

type
  TfmCodeOptions = class(TfmBaseForm)
    btnOK: TButton;
    btnCancel: TButton;
    pgeCodeOpt: TPageControl;
    tabPaths: TTabSheet;
    tabFonts: TTabSheet;
    lblStoragePath: TLabel;
    edPath: TEdit;
    sbBrowse: TButton;
    grp_Fonts: TGroupBox;
    lblTreeView: TLabel;
    lblEditor: TLabel;
    lblSize: TLabel;
    fcTreeview: TComboBox;
    fcEditor: TComboBox;
    udTreeview: TUpDown;
    udEditor: TUpDown;
    eTreeview: TEdit;
    eEditor: TEdit;
    grp_Layout: TGroupBox;
    pnlSideSide: TPanel;
    shpLeft: TShape;
    shpRight: TShape;
    pnlTopBottom: TPanel;
    shpTop: TShape;
    shpBottom: TShape;
    rbTop: TRadioButton;
    rbSide: TRadioButton;
    procedure sbBrowseClick(Sender: TObject);
    procedure eNumericKeyPress(Sender: TObject; var Key: Char);
  private
    procedure InitializeForm;
    procedure edPathOnFilesDropped(_Sender: TObject; _Files: TStrings);
    procedure SetData(const _StoragePath: string; _Layout: TCodeLayout;
      const _TreeFont: string; _TreeFontSize: Integer; const _EditFont: string; _EditFontSize: Integer);
    procedure GetData(out _StoragePath: string; out _Layout: TCodeLayout;
      out _TreeFont: string; out _TreeFontSize: Integer; out _EditFont: string; out _EditFontSize: Integer);
  public
    class function Execute(_Owner: TWinControl; var _StoragePath: string; var _Layout: TCodeLayout;
      var _TreeFont: string; var _TreeFontSize: Integer; var _EditFont: string; var _EditFontSize: Integer): Boolean;
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  SysUtils, GX_GenericUtils, GX_IdeUtils, u_dzVclUtils;

class function TfmCodeOptions.Execute(_Owner: TWinControl; var _StoragePath: string; var _Layout: TCodeLayout;
  var _TreeFont: string; var _TreeFontSize: Integer; var _EditFont: string; var _EditFontSize: Integer): Boolean;
var
  frm: TfmCodeOptions;
begin
  frm := TfmCodeOptions.Create(nil);
  try
    TForm_CenterOn(frm, _Owner);
    frm.SetData(_StoragePath, _Layout, _TreeFont, _TreeFontSize, _EditFont, _EditFontSize);
    Result :=( frm.ShowModal = mrOk);
    if Result then
      frm.GetData(_StoragePath, _Layout, _TreeFont, _TreeFontSize, _EditFont, _EditFontSize);
  finally
    FreeAndNil(frm);
  end;
end;

procedure TfmCodeOptions.SetData(const _StoragePath: string; _Layout: TCodeLayout;
  const _TreeFont: string; _TreeFontSize: Integer; const _EditFont: string; _EditFontSize: Integer);
begin
  edPath.Text := _StoragePath;

  if _Layout = clSide then
    rbSide.Checked := True
  else
    rbTop.Checked := True;

  fcTreeview.ItemIndex := fcTreeview.Items.IndexOf(_TreeFont);
  udTreeview.Position := _TreeFontSize;
  eTreeview.Text      := IntToStr(_TreeFontSize);

  fcEditor.ItemIndex := fcEditor.Items.IndexOf(_EditFont);
  udEditor.Position := _EditFontSize;
  eEditor.Text      := IntToStr(_EditFontSize);
end;

procedure TfmCodeOptions.GetData(out _StoragePath: string; out _Layout: TCodeLayout;
  out _TreeFont: string; out _TreeFontSize: Integer; out _EditFont: string; out _EditFontSize: Integer);
begin
  _StoragePath := edPath.Text;
  if rbSide.Checked then
    _Layout := clSide
  else
    _Layout := clTop;

  _TreeFont:= fcTreeview.Text;
  _TreeFontSize := Trunc(StrToInt(eTreeview.Text));
  _EditFont:= fcEditor.Text;
  _EditFontSize := Trunc(StrToInt(eEditor.Text));
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

  procedure SetShapeColors(AShape: TShape; APenColor, ABrushColor: TColor);
  begin
    AShape.Pen.Color   := APenColor;
    AShape.Brush.Color := ABrushColor;
  end;

var
  LPenColor   : TColor;
  LBrushColor : TColor;

begin
  fcTreeview.Items.Assign(Screen.Fonts);
  fcEditor.Items.Assign(Screen.Fonts);

  LPenColor   := GetThemedColor(clWindowFrame);
  LBrushColor := GetThemedColor(clWindow);

  SetShapeColors(shpLeft, LPenColor, LBrushColor);
  SetShapeColors(shpRight, LPenColor, LBrushColor);
  SetShapeColors(shpTop, LPenColor, LBrushColor);
  SetShapeColors(shpBottom, LPenColor, LBrushColor);
end;

constructor TfmCodeOptions.Create(_Owner: TComponent);
begin
  inherited;

  pgeCodeOpt.ActivePage := tabFonts;
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
