unit GX_FavWuppdiWPImport;

{$I GX_CondDefine.inc}

interface

uses
  Windows,
  SysUtils,
  Types,
  Classes,
  Controls,
  Forms,
  StdCtrls,
  GX_BaseForm;

type
  TfmFavWuppdiWPImport = class(TfmBaseForm)
    btnCancel: TButton;
    btnOK: TButton;
    lblWuppdiWPFilename: TLabel;
    edtWuppdiWPFilename: TEdit;
    btWuppdiWPFilenameSelect: TButton;
    edtSubfolderName: TEdit;
    lblSubfolderName: TLabel;

    procedure btWuppdiWPFilenameSelectClick(Sender: TObject);
    procedure edtWuppdiWPFilenameChange(Sender: TObject);
  private
    procedure InitializeForm;

    procedure FileDropWuppdiWPFile(_Sender: TObject; _Files: TStrings);
 protected
    procedure ArrangeControls; override;
  public
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  Graphics,
  Dialogs,
  u_dzTypesUtils,
  u_dzVclUtils,
{$IFDEF IDE_IS_HIDPI_AWARE}
  u_dzDpiScaleUtils,
{$ENDIF}
  GX_FavUtil,
  GX_FavFiles,
  GX_GenericUtils,
  GX_VerDepConst;

{ TfmFavWuppdiWPImport }

constructor TfmFavWuppdiWPImport.Create(_Owner: TComponent);
begin
  inherited;

  InitDpiScaler;

  InitializeForm;
end;

procedure TfmFavWuppdiWPImport.ArrangeControls;
begin
  inherited;
  lblSubfolderName.Left := edtSubfolderName.Left;
  lblWuppdiWPFilename.Left := edtWuppdiWPFilename.Left;
end;

procedure TfmFavWuppdiWPImport.btWuppdiWPFilenameSelectClick(Sender: TObject);
var
  OpenDlg: TOpenDialog;
  InitDir: string;
begin
  InitDir := '';

  // If there is a filename in the edit field the
  if (Trim(edtWuppdiWPFilename.Text) <> '') then
    InitDir := ExtractFilePath(edtWuppdiWPFilename.Text);

  if (Trim(InitDir) = '') or (not DirectoryExists(InitDir)) then begin
    // The folder does not exists (maybe it's a version of Delphi not supported by WuppdiWP),
    // so use the base directroy of the current Delphi version (like %appdata%\Embarcadero).
    InitDir := GetUserApplicationDataFolder + '\' + CompanyRegPrefix + 'BDS';

    // The suffix "BDS" does not exist, so it's either Delphi 7 or lower or something brand new
    if not DirectoryExists(InitDir) then
      InitDir := GetUserApplicationDataFolder + '\' + CompanyRegPrefix;

    // If this does also not exist, use the %appdata%-directory as a starting point
    if not DirectoryExists(InitDir) then
      InitDir := GetUserApplicationDataFolder;
  end;

  OpenDlg := TOpenDialog.Create(nil);
  try
    OpenDlg.InitialDir := InitDir;
    OpenDlg.Options := [ofReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing];
    OpenDlg.Filter := 'WuppdiWP configuration file|wuppdiwp.ini|All INI files|*.ini|All files|*.*';

    if OpenDlg.Execute then
      edtWuppdiWPFilename.Text := OpenDlg.Filename;
  finally
    FreeAndNil(OpenDlg);
  end;
end;

procedure TfmFavWuppdiWPImport.edtWuppdiWPFilenameChange(Sender: TObject);
begin
  btnOK.Enabled := (Length(edtWuppdiWPFilename.Text) > 0) and FileExists(edtWuppdiWPFilename.Text);
end;

procedure TfmFavWuppdiWPImport.FileDropWuppdiWPFile(_Sender: TObject; _Files: TStrings);
begin
  if Assigned(_Files) and (_Files.Count > 0) then
    edtWuppdiWPFilename.Text := _Files[0];
end;

procedure TfmFavWuppdiWPImport.InitializeForm;
var
  Filename: string;
begin
  TControl_SetMinConstraints(Self);

  // Try to get the filename in the directory of the current running Delphi version
  // If it does not exist leave it empty
  Filename := GetUserApplicationDataFolder + '\' + CompilerDefinedProductRegistryKey + '\wuppdiwp.ini';
  if FileExists(Filename) then
    edtWuppdiWPFilename.Text := Filename;

  TEdit_ActivateAutoComplete(edtWuppdiWPFilename);
  TWinControl_ActivateDropFiles(edtWuppdiWPFilename, FileDropWuppdiWPFile);

  edtSubfolderName.Text := 'WuppdiWP';
end;

end.

