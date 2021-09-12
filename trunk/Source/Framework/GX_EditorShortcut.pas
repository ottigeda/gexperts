unit GX_EditorShortcut;

interface

uses
  Classes, Controls, StdCtrls, ComCtrls, Forms, GX_BaseForm;

type
  TfmEditorShortcut = class(TfmBaseForm)
    gbxShortCut: TGroupBox;
    lblShortCut: TLabel;
    btnCancel: TButton;
    btnOK: TButton;
    hkyShortCut: THotKey;
    btnDefault: TButton;
    procedure btnDefaultClick(Sender: TObject);
  public
    FDefault: TShortCut;
    class function Execute(Owner: TWinControl; const Expert: string; var ShortCut: TShortCut;
      const Default: TShortCut): Boolean;
  end;

implementation

{$R *.dfm}

uses
  SysUtils, Menus, u_dzVclUtils;

{ TfmEditorShortcut }

class function TfmEditorShortcut.Execute(Owner: TWinControl; const Expert: string;
  var ShortCut: TShortCut; const Default: TShortCut): Boolean;
var
  frm: TfmEditorShortcut;
  Int: IInterface;
begin
  // This buys (me) some time with adapting forms for High DPI by temporarily turning off
  // High DPI awareness. Works only for forms that are shown modally and don't
  // call into the IDE before closing.
  // All this is only necessary for Delphi 11 and later.
  // It does nothing for older Delphi versions.
  int := TemporarilyDisableHighDpi;
  frm := TfmEditorShortcut.Create(Owner);
  try
    frm.TemporarilyDisableHighDpiInterface := int;
    Int := nil;
    frm.FDefault := Default;
    frm.gbxShortCut.Caption := Expert;
    THotkey_SetHotkey(frm.hkyShortCut, ShortCut);
    frm.btnDefault.Caption := ShortCutToText(Default);
    Result := (frm.ShowModal = mrOk);
    if Result then
      ShortCut := THotkey_GetHotkey(frm.hkyShortCut);
  finally
    FreeAndNil(frm);
  end;
end;

procedure TfmEditorShortcut.btnDefaultClick(Sender: TObject);
begin
  hkyShortCut.HotKey := FDefault;
end;

end.

