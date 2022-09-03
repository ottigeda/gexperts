unit GX_FavNewFolder;

{$I GX_CondDefine.inc}

interface

uses
  Windows, SysUtils, Types, Classes, Controls, Forms, StdCtrls, GX_BaseForm,
  GX_FavUtil;

type
  TfmFavNewFolder = class(TfmBaseForm)
    gbxNewFolder: TGroupBox;
    lblFolderName: TLabel;
    edtFolderName: TEdit;
    lblFolderType: TLabel;
    cbxFolderType: TComboBox;
    btnCancel: TButton;
    btnOK: TButton;
    procedure edtFolderNameChange(Sender: TObject);
    procedure cbxFolderTypeDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure cbxFolderTypeMeasureItem(Control: TWinControl; Index: Integer;
      var Height: Integer);
  private
    procedure InitializeForm;
    function DpiScaleValue(_Value: Integer): Integer;
    procedure GetData(out _FolderName: string; out _FolderType: TFolderType);
  protected
    FFavoriteFilesForm: TForm;
  public
    class function Execute(_Owner: TWinControl; out _FolderName: string; out _FolderType: TFolderType): Boolean;
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  Graphics,
  u_dzTypesUtils,
{$IFDEF IDE_IS_HIDPI_AWARE}
  u_dzDpiScaleUtils,
{$ENDIF}
  u_dzVclUtils,
  GX_FavFiles;

{ TfmFavNewFolder }

class function TfmFavNewFolder.Execute(_Owner: TWinControl; out _FolderName: string; out _FolderType: TFolderType): Boolean;
var
  frm: TfmFavNewFolder;
begin
  frm := Self.Create(_Owner);
  try
   TForm_CenterOn(frm, _Owner);
    Result := (frm.ShowModal = mrOk);
    if Result then
      frm.GetData(_FolderName, _FolderType);
  finally
    FreeAndNil(frm);
  end;
end;

constructor TfmFavNewFolder.Create(_Owner: TComponent);
begin
  inherited;

  InitDpiScaler;

  InitializeForm;
end;

procedure TfmFavNewFolder.edtFolderNameChange(Sender: TObject);
begin
  btnOK.Enabled := (Length(edtFolderName.Text) > 0);
end;

procedure TfmFavNewFolder.InitializeForm;
var
  i: TFolderType;
begin
  for i := Low(TFolderType) to High(TFolderType) do
    cbxFolderType.Items.AddObject(FolderNames[i], Pointer(i));
  cbxFolderType.ItemIndex := 0;
end;

function TfmFavNewFolder.DpiScaleValue(_Value: Integer): Integer;
begin
{$IFDEF IDE_IS_HIDPI_AWARE}
  Result := FScaler.Calc(_Value);
{$ELSE}
  Result := _Value;
{$ENDIF}
end;

procedure TfmFavNewFolder.GetData(out _FolderName: string; out _FolderType: TFolderType);
begin
  _FolderName := edtFolderName.Text;
  _FolderType := TFolderType(cbxFolderType.ItemIndex);
end;

procedure TfmFavNewFolder.cbxFolderTypeDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  cnv: TCanvas;
  il: TImageList;
  YOffset: Integer;
  TheForm: TfmFavFiles;
begin
  try
    cnv := cbxFolderType.Canvas;
    if odSelected in State then
      cnv.Brush.Color := clHighlight
    else
      cnv.Brush.Color := clWindow;
    cnv.FillRect(Rect);
    TheForm := (FFavoriteFilesForm as TfmFavFiles);
    il := TheForm.ilFolders;
    YOffset := (TRect_Height(Rect) - il.Height) div 2;
    il.Draw(cnv, Rect.Left + DpiScaleValue(3), Rect.Top + YOffset, Index * 2);
    YOffset := (TRect_Height(Rect) - cnv.TextHeight('Mg')) div 2;
    cnv.TextOut(Rect.Left + DpiScaleValue(22), Rect.Top + YOffset, cbxFolderType.Items[Index]);
  except
    on E: Exception do
    begin
      // ignore
    end;
  end;
end;

procedure TfmFavNewFolder.cbxFolderTypeMeasureItem(Control: TWinControl;
  Index: Integer; var Height: Integer);
begin
  Height := DpiScaleValue(18);
end;

end.
