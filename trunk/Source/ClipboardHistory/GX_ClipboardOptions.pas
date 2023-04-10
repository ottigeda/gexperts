unit GX_ClipboardOptions;

interface

uses
  SysUtils,
  Classes,
  Graphics,
  StdCtrls,
  Controls,
  ComCtrls,
  Forms,
  GX_BaseForm;

const
  CLIPBOARD_VIEWER_ENTRIES_MIN = 20;
  CLIPBOARD_VIEWER_ENTRIES_MAX = 1000;

type
  TfmClipboardOptions = class(TfmBaseForm)
    btnOK: TButton;
    btnCancel: TButton;
    chkAutoClose: TCheckBox;
    chkAutoStart: TCheckBox;
    lblMaxEntries: TLabel;
    edtMaxClip: TEdit;
    lbFont: TLabel;
    cbPreviewFont: TComboBox;
    edPreviewFont: TEdit;
    udPreviewFont: TUpDown;
    btnPreviewFont: TButton;
    txtPreview: TStaticText;
    b_Defaults: TButton;
    procedure btnPreviewFontClick(Sender: TObject);
    procedure edPreviewFontChange(Sender: TObject);
    procedure cbPreviewFontChange(Sender: TObject);
    procedure b_DefaultsClick(Sender: TObject);
  private
    procedure SetData(_MaxEntries: Integer; _AutoStart, _AutoClose: Boolean; _Font: TFont);
    procedure GetData(out _MaxEntries: Integer; out _AutoStart, _AutoClose: Boolean; _Font: TFont);
  public
    class function Execute(_Owner: TWinControl;
      var _MaxEntries: Integer; var _AutoStart, _AutoClose: Boolean; _Font: TFont): Boolean;
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  Math,
  Dialogs,
  u_dzVclUtils;

{ TfmClipboardOptions }

class function TfmClipboardOptions.Execute(_Owner: TWinControl;
  var _MaxEntries: Integer; var _AutoStart, _AutoClose: Boolean; _Font: TFont): Boolean;
var
  frm: TfmClipboardOptions;
begin
  frm := TfmClipboardOptions.Create(_Owner);
  try
    frm.SetData(_MaxEntries, _AutoStart, _AutoClose, _Font);
    Result := (frm.ShowModal = mrOk);
    if Result then
      frm.GetData(_MaxEntries, _AutoStart, _AutoClose, _Font);
  finally
    FreeAndNil(frm);
  end;
end;

constructor TfmClipboardOptions.Create(_Owner: TComponent);
begin
  inherited;

  cbPreviewFont.Items.Assign(Screen.Fonts);

  InitDpiScaler;
end;

procedure TfmClipboardOptions.GetData(out _MaxEntries: Integer; out _AutoStart, _AutoClose: Boolean;
  _Font: TFont);
var
  FontName: string;
begin
  _MaxEntries := Min(StrToIntDef(edtMaxClip.Text, CLIPBOARD_VIEWER_ENTRIES_MIN), CLIPBOARD_VIEWER_ENTRIES_MAX);
  _AutoStart := chkAutoStart.Checked;
  _AutoClose := chkAutoClose.Checked;

  _Font.Assign(txtPreview.Font);
  if TComboBox_GetSelected(cbPreviewFont, FontName) then
    _Font.Name := FontName;
  _Font.Size := udPreviewFont.Position;
end;

procedure TfmClipboardOptions.SetData(_MaxEntries: Integer; _AutoStart, _AutoClose: Boolean;
  _Font: TFont);
begin
  edtMaxClip.Text := IntToStr(_MaxEntries);
  chkAutoStart.Checked := _AutoStart;
  chkAutoClose.Checked := _AutoClose;
  txtPreview.Font.Assign(_Font);
  TComboBox_Select(cbPreviewFont, txtPreview.Font.Name);
  udPreviewFont.Position := txtPreview.Font.Size;
end;

procedure TfmClipboardOptions.btnPreviewFontClick(Sender: TObject);
var
  Dlg: TFontDialog;
begin
  Dlg := TFontDialog.Create(Self);
  try
    Dlg.Font := txtPreview.Font;
    if Dlg.Execute then begin
      txtPreview.Font.Assign(Dlg.Font);
      TComboBox_Select(cbPreviewFont, txtPreview.Font.Name);
      udPreviewFont.Position := txtPreview.Font.Size;
    end;
  finally
    FreeAndNil(Dlg);
  end;
end;

procedure TfmClipboardOptions.b_DefaultsClick(Sender: TObject);
begin
  edtMaxClip.Text := IntToStr(CLIPBOARD_VIEWER_ENTRIES_MIN);
  chkAutoStart.Checked := True;
  chkAutoClose.Checked := True;
  txtPreview.ParentFont := True;
end;

procedure TfmClipboardOptions.cbPreviewFontChange(Sender: TObject);
var
  FontName: string;
begin
  if TComboBox_GetSelected(cbPreviewFont, FontName) then
    txtPreview.Font.Name := FontName;
end;

procedure TfmClipboardOptions.edPreviewFontChange(Sender: TObject);
begin
  txtPreview.Font.Size := udPreviewFont.Position;
end;

end.
