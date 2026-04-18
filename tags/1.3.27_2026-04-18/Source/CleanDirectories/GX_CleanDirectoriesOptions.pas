unit GX_CleanDirectoriesOptions;

{$I GX_CondDefine.inc}

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  GX_BaseForm;

type
  TfmCleanDirectoriesOptions = class(TfmBaseForm)
    chk_IncludeBinary: TCheckBox;
    b_OK: TButton;
    b_Cancel: TButton;
  private
    procedure SetData(_IncludeBinaryDirs: Boolean);
    procedure GetData(out _IncludeBinaryDirs: Boolean);
  public
    class function Execute(_Owner: TWinControl; var _IncludeBinaryDirs: Boolean): Boolean;
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  u_dzVclUtils;

{ TfmCleanDirectoriesOptions }

class function TfmCleanDirectoriesOptions.Execute(_Owner: TWinControl; var _IncludeBinaryDirs: Boolean): Boolean;
var
  frm: TfmCleanDirectoriesOptions;
begin
  frm := TfmCleanDirectoriesOptions.Create(_Owner);
  try
    TForm_CenterOn(frm, _Owner);
    frm.SetData(_IncludeBinaryDirs);
    Result := (frm.ShowModal = mrOk);
    if Result then
      frm.GetData(_IncludeBinaryDirs);
  finally
    FreeAndNil(frm);
  end;
end;

constructor TfmCleanDirectoriesOptions.Create(_Owner: TComponent);
begin
  inherited;
  TControl_SetMinConstraints(Self);
  InitDpiScaler;
end;

procedure TfmCleanDirectoriesOptions.GetData(out _IncludeBinaryDirs: Boolean);
begin
  _IncludeBinaryDirs := chk_IncludeBinary.Checked;
end;

procedure TfmCleanDirectoriesOptions.SetData(_IncludeBinaryDirs: Boolean);
begin
  chk_IncludeBinary.Checked := _IncludeBinaryDirs;
end;

end.

