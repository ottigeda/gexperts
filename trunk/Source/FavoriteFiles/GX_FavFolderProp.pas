unit GX_FavFolderProp;

interface

uses
  SysUtils,
  Classes,
  Forms,
  Controls,
  StdCtrls,
  GX_FavNewFolder,
  GX_FavUtil;

type
  TfmFavFolderProperties = class(TfmFavNewFolder)
  private
    procedure SetData(_FavoriteFilesForm: TForm; _Folder: TGXFolder); reintroduce;
    procedure GetData(_Folder: TGXFolder); reintroduce;
  public
    class function Execute(_Owner: TWinControl; _Folder: TGXFolder): Boolean; reintroduce;
  end;

implementation

{$R *.dfm}

uses
  u_dzVclUtils;

{ TfmFavFolderProperties }

class function TfmFavFolderProperties.Execute(_Owner: TWinControl; _Folder: TGXFolder): Boolean;
var
  frm: TfmFavFolderProperties;
begin
  frm := TfmFavFolderProperties.Create(_Owner);
  try
    TForm_CenterOn(frm, _Owner);
    frm.SetData(_Owner as TForm, _Folder);
    Result := (frm.ShowModal = mrOk);
    if Result then
      frm.GetData(_Folder);
  finally
    FreeAndNil(frm);
  end;
end;

procedure TfmFavFolderProperties.GetData(_Folder: TGXFolder);
begin
  _Folder.FolderName := edtFolderName.Text;
  _Folder.FolderType := TFolderType(cbxFolderType.ItemIndex);
end;

procedure TfmFavFolderProperties.SetData(_FavoriteFilesForm: TForm; _Folder: TGXFolder);
begin
  FFavoriteFilesForm := _FavoriteFilesForm;
  edtFolderName.Text := _Folder.FolderName;
  cbxFolderType.ItemIndex := Ord(_Folder.FolderType);
end;

end.
