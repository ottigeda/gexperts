unit GX_GrepOptions;

interface

uses
  SysUtils, Classes, Controls, StdCtrls, Forms, GX_BaseForm;

type
  TfmGrepOptions = class(TfmBaseForm)
    gbxOptions: TGroupBox;
    btnOK: TButton;
    btnCancel: TButton;
    chkGrepUseCurrentIdent: TCheckBox;
    chkUseMapFile: TCheckBox;
  public
    class function Execute(var UseCurrentIdent, UseMapFile: Boolean): Boolean;
  end;

implementation

{$R *.dfm}

{ TfmGrepOptions }

class function TfmGrepOptions.Execute(var UseCurrentIdent, UseMapFile: Boolean): Boolean;
var
  Dlg: TfmGrepOptions;
begin
  Dlg := TfmGrepOptions.Create(nil);
  try
    Dlg.chkGrepUseCurrentIdent.Checked := UseCurrentIdent;
    Dlg.chkUseMapFile.Checked := UseMapFile;
    Result := (Dlg.ShowModal = mrOk);
    if Result then
    begin
      UseCurrentIdent := Dlg.chkGrepUseCurrentIdent.Checked;
      UseMapFile := Dlg.chkUseMapFile.Checked;
    end;
  finally
    FreeAndNil(Dlg);
  end;
end;

end.
