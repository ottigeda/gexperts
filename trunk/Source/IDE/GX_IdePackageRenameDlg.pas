// This file is unused, for now
unit GX_IdePackageRenameDlg;

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
  StdCtrls;

type
  TfmIdxPackageRenameDlg = class(TForm)
    l_PackageFn: TLabel;
    l_Description: TLabel;
    ed_Description: TEdit;
    b_Ok: TButton;
    b_Cancel: TButton;
  private
    procedure SetData(const _fn: string; const _Description: string);
    procedure GetData(out _Description: string);
  public
    class function Execute(_Owner: TWinControl; const _fn: string; var _Description: string): Boolean;
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  u_dzVclUtils;

{ TfmIdxPackageRenameDlg }

class function TfmIdxPackageRenameDlg.Execute(_Owner: TWinControl; const _fn: string;
  var _Description: string): Boolean;
var
  frm: TfmIdxPackageRenameDlg;
  Int: IInterface;
begin
  // This buys (me) some time with adapting forms for High DPI by temporarily turning off
  // High DPI awareness. Works only for forms that are shown modally and don't
  // call into the IDE before closing.
  // All this is only necessary for Delphi 11 and later.
  // It does nothing for older Delphi versions.
  int := TemporarilyDisableHighDpi;
  frm := TfmIdxPackageRenameDlg.Create(_Owner);
  try
    frm.TemporarilyDisableHighDpiInterface := int;
    Int := nil;
    frm.SetData(_fn, _Description);
    Result := (frm.ShowModal = mrok);
    if Result then begin
      frm.GetData(_Description);
    end;
  finally
    FreeAndNil(frm);
  end;
end;

constructor TfmIdxPackageRenameDlg.Create(_Owner: TComponent);
begin
  inherited;
  TControl_SetMinConstraints(Self);
end;

procedure TfmIdxPackageRenameDlg.GetData(out _Description: string);
begin
  _Description := ed_Description.Text;
end;

procedure TfmIdxPackageRenameDlg.SetData(const _fn, _Description: string);
begin
  l_PackageFn.Caption := _fn;
  ed_Description.Text := _Description;
end;

end.
