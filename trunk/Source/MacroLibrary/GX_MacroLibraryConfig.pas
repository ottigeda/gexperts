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
  Int: IInterface;
begin
  // This buys (me) some time with adapting forms for High DPI by temporarily turning off
  // High DPI awareness. Works only for forms that are shown modally and don't
  // call into the IDE before closing.
  // All this is only necessary for Delphi 11 and later.
  // It does nothing for older Delphi versions.
  int := TemporarilyDisableHighDpi;
  frm := TfmGxMacroLibraryConfig.Create(nil);
  try
    frm.TemporarilyDisableHighDpiInterface := int;
    Int := nil;
    frm.chk_AutoPrompt.Checked := APromptForName;
    Result := mrOk = frm.ShowModal;
    if Result then
      APromptForName := frm.chk_AutoPrompt.Checked;
  finally
    FreeAndNil(frm);
  end;
end;

end.
