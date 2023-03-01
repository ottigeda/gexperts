unit GX_MacroLibraryConfig;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  GX_BaseForm;

type
  TfmGxMacroLibraryConfig = class(TfmBaseForm)
    chk_AutoPrompt: TCheckBox;
    b_Ok: TButton;
    b_Cancel: TButton;
  private
  public
    class function Execute(var APromptForName: Boolean): Boolean;
  end;

implementation

{$R *.dfm}

uses
  u_dzVclUtils;

{ TfmGxMacroLibraryConfig }

class function TfmGxMacroLibraryConfig.Execute(var APromptForName: Boolean): Boolean;
var
  frm: TfmGxMacroLibraryConfig;
begin
  frm := TfmGxMacroLibraryConfig.Create(nil);
  try
    frm.chk_AutoPrompt.Checked := APromptForName;
    frm.InitDpiScaler;
    Result := mrOk = frm.ShowModal;
    if Result then
      APromptForName := frm.chk_AutoPrompt.Checked;
  finally
    FreeAndNil(frm);
  end;
end;

end.
