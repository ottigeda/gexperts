unit GX_FavFileProp;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls, StdCtrls, ComCtrls,
  GX_BaseForm, GX_FavUtil;

type
  TfmFavFileProp = class(TfmBaseForm)
    pgeProperties: TPageControl;
    tabProperties: TTabSheet;
    lblFile: TLabel;
    lblName: TLabel;
    lblDescription: TLabel;
    btnCancel: TButton;
    btnOK: TButton;
    edtName: TEdit;
    edtDescription: TEdit;
    imgFileIcon: TImage;
    lblIcon: TLabel;
    lblExecuteType: TLabel;
    cbxExecuteType: TComboBox;
    edtFilename: TEdit;
    sbnFile: TButton;
    lblExecuteUsing: TLabel;
    edtExecuteUsing: TEdit;
    sbnExecute: TButton;
    procedure sbnFileClick(Sender: TObject);
    procedure edtFilenameExit(Sender: TObject);
    procedure sbnExecuteClick(Sender: TObject);
    procedure cbxExecuteTypeClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    // We need this to call TfmFavFiles.MakeFileNameAbsolute, .MakeFilenameRelative and .SetFilter
    // and also to access .dlgGetFiles. All that is far from ideal
    FFavoriteFilesForm: TForm;
    procedure InitializeForm;
    procedure SetData(_FavoriteFilesForm: TForm; _File: TGXFile);
    procedure GetData(_File: TGXFile);
  public
    class function Execute(_Owner: TWinControl; _File: TGXFile): boolean;
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  GX_FavFiles, GX_GenericUtils, u_dzVclUtils;

{ TfmFavFileProp }

class function TfmFavFileProp.Execute(_Owner: TWinControl; _File: TGXFile): boolean;
var
  frm: TfmFavFileProp;
begin
  frm := TfmFavFileProp.Create(_Owner);
  try
    TForm_CenterOn(frm, _Owner);
    frm.SetData(_Owner as TForm, _File);
    Result := ( frm.ShowModal = mrOk );
    if Result then begin
      frm.GetData(_File);
    end;
  finally
    FreeAndNil(frm);
  end;
end;

constructor TfmFavFileProp.Create(_Owner: TComponent);
begin
  inherited;

  InitDpiScaler;

  InitializeForm;
end;

procedure TfmFavFileProp.sbnFileClick(Sender: TObject);
var
  TheForm: TfmFavFiles;
  FileName: string;
begin
  TheForm := (FFavoriteFilesForm as TfmFavFiles);
  TheForm.SetFilter;

  FileName := TheForm.MakeFileNameAbsolute(edtFileName.Text);
  if FileExists(FileName) then
  begin
    TheForm.dlgGetFiles.FileName := ExtractFileName(FileName);
    TheForm.dlgGetFiles.InitialDir := ExtractFilePath(FileName);
  end
  else
    TheForm.dlgGetFiles.FileName := '';

  if TheForm.dlgGetFiles.Execute then
  begin
    FileName := TheForm.dlgGetFiles.FileName;
    edtFilename.Text := TheForm.MakeFileNameRelative(FileName);
    AssignIconToImage(FileName, imgFileIcon);
  end;
end;

procedure TfmFavFileProp.edtFilenameExit(Sender: TObject);
var
  TheForm: TfmFavFiles;
begin
  TheForm := (FFavoriteFilesForm as TfmFavFiles);
  AssignIconToImage(TheForm.MakeFileNameAbsolute(edtFilename.Text), imgFileIcon);
end;

procedure TfmFavFileProp.sbnExecuteClick(Sender: TObject);
var
  fn: string;
begin
  fn := edtExecuteUsing.Text;
  if ShowOpenDialog('Select application', '.exe', fn, 'Executable Files (*.exe)|*.exe') then
    edtExecuteUsing.Text := fn;
end;

procedure TfmFavFileProp.cbxExecuteTypeClick(Sender: TObject);
var
  ItemEnabled: Boolean;
begin
  ItemEnabled := (cbxExecuteType.ItemIndex = 2);

  sbnExecute.Enabled := ItemEnabled;
  edtExecuteUsing.Enabled := ItemEnabled;
  if ItemEnabled then
    edtExecuteUsing.Color := clWindow
  else
    edtExecuteUsing.Color := clBtnFace;
end;

procedure TfmFavFileProp.FormActivate(Sender: TObject);
begin
  cbxExecuteTypeClick(cbxExecuteType);
end;

procedure TfmFavFileProp.GetData(_File: TGXFile);
begin
  _File.FileName := edtFilename.Text;
  _File.Description := edtDescription.Text;
  _File.DName := edtName.Text;
  _File.ExecType := TExecType(cbxExecuteType.ItemIndex);
  _File.ExecProg := edtExecuteUsing.Text;
end;

procedure TfmFavFileProp.SetData(_FavoriteFilesForm: TForm; _File: TGXFile);
var
  TheForm: TfmFavFiles;
begin
  FFavoriteFilesForm := _FavoriteFilesForm;
  TheForm := (FFavoriteFilesForm as TfmFavFiles);
  edtFilename.Text := _File.FileName;
  edtName.Text := _File.DName;
  edtDescription.Text := _File.Description;
  cbxExecuteType.ItemIndex := Ord(_File.ExecType);
  edtExecuteUsing.Text := _File.ExecProg;
  AssignIconToImage(TheForm.MakeFileNameAbsolute(_File.FileName), imgFileIcon);
end;

procedure TfmFavFileProp.InitializeForm;
var
  ExecType: TExecType;
begin
  cbxExecuteType.Items.Clear;
  for ExecType := Low(TExecType) to High(TExecType) do
    cbxExecuteType.Items.AddObject(ExecTypeNames[ExecType], TObject(Ord(ExecType)));
end;

end.

