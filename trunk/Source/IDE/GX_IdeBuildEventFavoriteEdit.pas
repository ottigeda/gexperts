unit GX_IdeBuildEventFavoriteEdit;

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
  Tf_IdeBuildEventFavoriteEdit = class(TfmBaseForm)
    l_Name: TLabel;
    ed_Name: TEdit;
    l_Command: TLabel;
    ed_Command: TEdit;
    b_OK: TButton;
    b_Cancel: TButton;
  private
    procedure GetData(out _Name, _Path: string);
    procedure SetData(const _Name, _Command: string);
    procedure HandleFilesDropped(_Sender: TObject; _Files: TStrings);
  public
    class function Execute(_Owner: TComponent; var _Name, _Command: string): Boolean;
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  u_dzVclUtils;

{ Tf_IdeBuildEventFavoriteEdit }

class function Tf_IdeBuildEventFavoriteEdit.Execute(_Owner: TComponent; var _Name, _Command: string): Boolean;
var
  frm: Tf_IdeBuildEventFavoriteEdit;
  Int: IInterface;
begin
  // This buys (me) some time with adapting forms for High DPI by temporarily turning off
  // High DPI awareness. Works only for forms that are shown modally and don't
  // call into the IDE before closing.
  // All this is only necessary for Delphi 11 and later.
  // It does nothing for older Delphi versions.
  int := TemporarilyDisableHighDpi;
  frm := Tf_IdeBuildEventFavoriteEdit.Create(_Owner);
  try
    frm.TemporarilyDisableHighDpiInterface := int;
    Int := nil;
    frm.SetData(_Name, _Command);
    Result := (frm.ShowModal = mrok);
    if Result then
      frm.GetData(_Name, _Command);
  finally
    FreeAndNil(frm);
  end;
end;

constructor Tf_IdeBuildEventFavoriteEdit.Create(_Owner: TComponent);
begin
  inherited;

  TControl_SetConstraints(Self, [ccMinWidth, ccMinHeight, ccMaxHeight]);
  TWinControl_ActivateDropFiles(ed_Command, HandleFilesDropped);
end;

procedure Tf_IdeBuildEventFavoriteEdit.HandleFilesDropped(_Sender: TObject; _Files: TStrings);
begin
  ed_Command.Text := _Files[0];
end;

procedure Tf_IdeBuildEventFavoriteEdit.GetData(out _Name, _Path: string);
begin
  _Name := ed_Name.Text;
  _Path := ed_Command.Text;
end;

procedure Tf_IdeBuildEventFavoriteEdit.SetData(const _Name, _Command: string);
begin
  ed_Name.Text := _Name;
  ed_Command.Text := _Command;
end;

end.
