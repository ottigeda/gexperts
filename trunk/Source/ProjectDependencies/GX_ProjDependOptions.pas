unit GX_ProjDependOptions;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  GX_BaseForm,
  StdCtrls;

type
  TfmProjDependOptions = class(TfmBaseForm)
    chk_ScanEntireUnit: TCheckBox;
    l_ScanEntireUnitBla: TLabel;
    chk_SearchBrowsingPath: TCheckBox;
    l_ScanBrowsingPath: TLabel;
    b_Ok: TButton;
    b_Cancel: TButton;
    chk_SearchLibraryPath: TCheckBox;
    l_SearchLibraryPath: TLabel;
  private
    procedure SetData(_ScanEntireUnit, _SearchLibraryPath, _SearchBrowsingPath: Boolean);
    procedure GetData(out _ScanEntireUnit, _SearchLibraryPath, _SearchBrowsingPath: Boolean);
  public
    class function Execute(_Owner: TWinControl;
      var _ScanEntireUnit, _SearchLibraryPath, _SearchBrowsingPath: Boolean): Boolean;
  end;

implementation

{$R *.dfm}

uses
  u_dzVclUtils;

{ TfmProjDependOptions }

class function TfmProjDependOptions.Execute(_Owner: TWinControl;
  var _ScanEntireUnit, _SearchLibraryPath, _SearchBrowsingPath: Boolean): Boolean;
var
  frm: TfmProjDependOptions;
  Int: IInterface;
begin
  // This buys (me) some time with adapting forms for High DPI by temporarily turning off
  // High DPI awareness. Works only for forms that are shown modally and don't
  // call into the IDE before closing.
  // All this is only necessary for Delphi 11 and later.
  // It does nothing for older Delphi versions.
  int := TemporarilyDisableHighDpi;
  frm := TfmProjDependOptions.Create(_Owner);
  try
    frm.TemporarilyDisableHighDpiInterface := int;
    Int := nil;
    TForm_CenterOn(frm, _Owner);
    frm.SetData(_ScanEntireUnit, _SearchLibraryPath, _SearchBrowsingPath);
    Result := (frm.ShowModal = mrOk);
    if Result then
      frm.GetData(_ScanEntireUnit, _SearchLibraryPath, _SearchBrowsingPath);
  finally
    FreeAndNil(frm);
  end;
end;

procedure TfmProjDependOptions.GetData(out _ScanEntireUnit, _SearchLibraryPath, _SearchBrowsingPath: Boolean);
begin
  _ScanEntireUnit := chk_ScanEntireUnit.Checked;
  _SearchLibraryPath := chk_SearchLibraryPath.Checked;
  _SearchBrowsingPath := chk_SearchBrowsingPath.Checked;
end;

procedure TfmProjDependOptions.SetData(_ScanEntireUnit, _SearchLibraryPath, _SearchBrowsingPath: Boolean);
begin
  chk_ScanEntireUnit.Checked := _ScanEntireUnit;
  chk_SearchLibraryPath.Checked := _SearchLibraryPath;
  chk_SearchBrowsingPath.Checked := _SearchBrowsingPath;
end;

end.
