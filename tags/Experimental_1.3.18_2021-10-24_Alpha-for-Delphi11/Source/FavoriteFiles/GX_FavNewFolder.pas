unit GX_FavNewFolder;

{$I GX_CondDefine.inc}

interface

uses
  Windows, SysUtils, Types, Classes, Controls, Forms, StdCtrls, GX_BaseForm;

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
    procedure FormCreate(Sender: TObject);
    procedure cbxFolderTypeDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure cbxFolderTypeMeasureItem(Control: TWinControl; Index: Integer;
      var Height: Integer);
  private
    FFavoriteFilesForm: TForm;
    function DpiScaleValue(_Value: Integer): Integer;
  public
    constructor Create(_Owner: TComponent); override;
    property FavoriteFilesForm: TForm write FFavoriteFilesForm;
  end;

implementation

{$R *.dfm}

uses
  Graphics,
  u_dzTypesUtils,
{$IFDEF IDE_IS_HIDPI_AWARE}
  u_dzVclUtils,
  u_dzDpiScaleUtils,
{$ENDIF}
  GX_FavUtil, GX_FavFiles;

{ TfmFavNewFolder }

constructor TfmFavNewFolder.Create(_Owner: TComponent);
begin
  inherited;

  InitDpiScaler;
end;

procedure TfmFavNewFolder.edtFolderNameChange(Sender: TObject);
begin
  btnOK.Enabled := (Length(edtFolderName.Text) > 0);
end;

procedure TfmFavNewFolder.FormCreate(Sender: TObject);
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

procedure TfmFavNewFolder.cbxFolderTypeDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  cnv: TCanvas;
  il: TImageList;
  YOffset: Integer;
begin
  try
    cnv := cbxFolderType.Canvas;
    if odSelected in State then
      cnv.Brush.Color := clHighlight
    else
      cnv.Brush.Color := clWindow;
    cnv.FillRect(Rect);
    il := (FFavoriteFilesForm as TfmFavFiles).ilFolders;
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
