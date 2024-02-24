unit GX_FormHotkeysSelect;

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
  StdCtrls,
  ComCtrls;

type
  TfmFormHotkeysSelect = class(TfmBaseForm)
    b_Ok: TButton;
    b_Cancel: TButton;
    hk_Notkey: THotKey;
    l_HotkeyFor: TLabel;
  private
    procedure SetData(const _ComponentName: string; _Hotkey: TShortCut);
    procedure GetData(out _Hotkey: TShortCut);
  public
    class function Execute(_Owner: TWinControl; const _ComponentName: string; var _Hotkey: TShortCut): Boolean;
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  u_dzVclUtils;

{ TfmFormHotkeysSelect }

constructor TfmFormHotkeysSelect.Create(_Owner: TComponent);
begin
  inherited;
  InitDpiScaler;
end;

class function TfmFormHotkeysSelect.Execute(_Owner: TWinControl; const _ComponentName: string;
  var _Hotkey: TShortCut): Boolean;
var
  frm: TfmFormHotkeysSelect;
begin
  frm := TfmFormHotkeysSelect.Create(_Owner);
  try
    TForm_CenterOn(frm, _Owner);
    frm.SetData(_ComponentName, _Hotkey);
    Result := (mrOk = frm.ShowModal);
    if Result then
      frm.GetData(_Hotkey);
  finally
    FreeAndNil(frm);
  end;
end;

procedure TfmFormHotkeysSelect.GetData(out _Hotkey: TShortCut);
begin
  _Hotkey := hk_Notkey.HotKey;
end;

procedure TfmFormHotkeysSelect.SetData(const _ComponentName: string; _Hotkey: TShortCut);
begin
  l_HotkeyFor.Caption := Format('Hotkey for %s', [_ComponentName]);
  hk_Notkey.HotKey := _Hotkey;
end;

end.
