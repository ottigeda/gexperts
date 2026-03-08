unit GX_SourceExportOptions;

{$I GX_CondDefine.inc}

interface

uses
  Classes, Graphics, Controls, Forms, StdCtrls, ExtCtrls,
  SynEdit, Dialogs, GX_BaseForm;

type
  TfmSourceExportOptions = class(TfmBaseForm)
    pnlSettings: TPanel;
    dlgColor: TColorDialog;
    pnlButtons: TPanel;
    pnlButtonsRight: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    pnlCode: TPanel;
    gbxAttributes: TGroupBox;
    chkBold: TCheckBox;
    chkItalic: TCheckBox;
    chkUnderline: TCheckBox;
    chkStrikeOut: TCheckBox;
    cbxAttributes: TComboBox;
    rbxCopySettings: TRadioGroup;
    btnBackgroundColor: TButton;
    btnLoadIde: TButton;
    lblElement: TLabel;
    lblForeground: TLabel;
    lblBackground: TLabel;
    pnlForeground: TPanel;
    pnlBackground: TPanel;
    procedure AttributeChange(Sender: TObject);
    procedure cbxAttributesChange(Sender: TObject);
    procedure btnLoadIdeClick(Sender: TObject);
    procedure btnBackgroundColorClick(Sender: TObject);
    procedure pnlForegroundClick(Sender: TObject);
    procedure pnlBackgroundClick(Sender: TObject);
  private
    FSampleEditor: TSynEdit;
    NoChangeEvents: Boolean;
    procedure SynEditSelectionChange(Sender: TObject; Changes: TSynStatusChanges);
  public
    BackgroundColor: TColor;
    constructor Create(_Owner: TComponent); override;
    property SynSampleEditor: TSynEdit read FSampleEditor write FSampleEditor;
  end;

implementation

{$R *.dfm}

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  SysUtils, StrUtils,
  u_dzVclUtils, u_dzStringUtils,
  SynEditHighlighter, SynUnicode,
  GX_OtaUtils,
  GX_SynMemoUtils, GX_GenericUtils, GX_IdeUtils;

constructor TfmSourceExportOptions.Create(_Owner: TComponent);
var
  i: Integer;
  GXHighlighter: TGXSyntaxHighlighter;
begin
  inherited;

  FSampleEditor := TSynEdit.Create(Self);
  FSampleEditor.Parent := pnlCode;
  FSampleEditor.ReadOnly := True;
  FSampleEditor.Align := alClient;
  FSampleEditor.Gutter.Width := 0;
  FSampleEditor.Options := FSampleEditor.Options + [eoNoCaret, eoNoSelection, eoHideShowScrollbars, eoAutoSizeMaxScrollWidth] - [eoScrollPastEof, eoScrollPastEol];
  FSampleEditor.OnStatusChange := SynEditSelectionChange;
  Ota.GetEditorFont(FSampleEditor.Font);

  GXHighlighter := GetGXHighlighterForCurrentSourceEditor;

  SetSynEditHighlighter(FSampleEditor, GXHighlighter);
  FSampleEditor.Lines.Text := FSampleEditor.Highlighter.SampleSource;
  if Trim(FSampleEditor.Lines.Text) = '' then
    FSampleEditor.Lines.Text := 'No sample source available';
  if GXHighlighter in [gxpHTML, gxpSQL] then
    btnLoadIde.Enabled := False;

  BorderStyle := bsSizeable;
  TControl_SetMinConstraints(Self);

  btnLoadIdeClick(btnLoadIde);
  for i := 0 to FSampleEditor.Highlighter.AttrCount - 1 do
    cbxAttributes.Items.Add(FSampleEditor.Highlighter.Attribute[i].Name);
  cbxAttributes.ItemIndex := 0;
  cbxAttributesChange(Self);

  InitDpiScaler;
end;

// Bug #119: Replaced TColorGrid (16 colors only) with color panels + TColorDialog
// to support the full color palette.

procedure TfmSourceExportOptions.pnlForegroundClick(Sender: TObject);
begin
  dlgColor.Color := pnlForeground.Color;
  if dlgColor.Execute then
  begin
    pnlForeground.Color := dlgColor.Color;
    AttributeChange(nil);
  end;
end;

procedure TfmSourceExportOptions.pnlBackgroundClick(Sender: TObject);
begin
  dlgColor.Color := pnlBackground.Color;
  if dlgColor.Execute then
  begin
    pnlBackground.Color := dlgColor.Color;
    AttributeChange(nil);
  end;
end;

procedure TfmSourceExportOptions.AttributeChange(Sender: TObject);
var
  Attr: TSynHighlighterAttributes;
  AttrStyle: TFontStyles;
begin
  if NoChangeEvents then
    Exit;
  Attr := TSynHighlighterAttributes.Create(cbxAttributes.Items[cbxAttributes.ItemIndex],
    cbxAttributes.Items[cbxAttributes.ItemIndex]);
  try
    AttrStyle := [];
    Attr.Foreground := pnlForeground.Color;
    Attr.Background := pnlBackground.Color;
    if chkBold.Checked then
      Include(AttrStyle, fsBold);
    if chkItalic.Checked then
      Include(AttrStyle, fsItalic);
    if chkUnderline.Checked then
      Include(AttrStyle, fsUnderline);
    if chkStrikeOut.Checked then
      Include(AttrStyle, fsStrikeOut);
    Attr.Style := AttrStyle;
    FSampleEditor.Highlighter.Attribute[cbxAttributes.ItemIndex].Assign(Attr);
    FSampleEditor.Refresh;
  finally
    FreeAndNil(Attr);
  end;
end;

procedure TfmSourceExportOptions.cbxAttributesChange(Sender: TObject);
var
  Attr: TSynHighlighterAttributes;
begin
  Attr := TSynHighlighterAttributes.Create('', '');
  try
    Attr.Assign(FSampleEditor.Highlighter.Attribute[cbxAttributes.ItemIndex]);
    NoChangeEvents := True;
    try
      pnlForeground.Color := Attr.Foreground;
      pnlBackground.Color := Attr.Background;

      chkBold.Checked := (fsBold in Attr.Style);
      chkItalic.Checked := (fsItalic in Attr.Style);
      chkUnderline.Checked := (fsUnderline in Attr.Style);
      chkStrikeOut.Checked := (fsStrikeOut in Attr.Style);
    finally
      NoChangeEvents := False;
      AttributeChange(nil);
    end;
  finally
    FreeAndNil(Attr);
  end;
end;

procedure TfmSourceExportOptions.btnLoadIdeClick(Sender: TObject);
resourcestring
  ErrorLoadingIDESettings = 'Error loading IDE editor registry settings.  You may need to customize your editor highlighter settings before they can be loaded from the registry.';
var
  UserSettings: TStringList;
  i: Integer;
begin
  UserSettings := TStringList.Create;
  try
    FSampleEditor.Highlighter.EnumUserSettings(UserSettings);
    for i := UserSettings.Count - 1 downto 0 do
    begin
      if StartsStr(GetIDEVersionID, UserSettings[i]) then
      begin
        if not FSampleEditor.Highlighter.UseUserSettings(i) then
          MessageDlg(ErrorLoadingIDESettings, mtError, [mbOK], 0);
        Break;
      end;
    end;
  finally
    FreeAndNil(UserSettings);
  end;
  // Update color panels after loading IDE settings
  if cbxAttributes.ItemIndex >= 0 then
    cbxAttributesChange(Self);
end;

procedure TfmSourceExportOptions.SynEditSelectionChange(Sender: TObject; Changes: TSynStatusChanges);
var
  Token: UnicodeString;
  Attributes: TSynHighlighterAttributes;
  i: Integer;
begin
  FSampleEditor.GetHighlighterAttriAtRowCol(FSampleEditor.CaretXY, Token, Attributes);
  if Attributes = nil then
    Exit;
  for i := 0 to FSampleEditor.Highlighter.AttrCount - 1 do
  begin
    if Attributes.Name = FSampleEditor.Highlighter.Attribute[i].Name then
    begin
      cbxAttributes.ItemIndex := i;
      cbxAttributes.OnChange(Self);
      Break;
    end;
  end;
end;

procedure TfmSourceExportOptions.btnBackgroundColorClick(Sender: TObject);
begin
  dlgColor.Color := BackgroundColor;
  if dlgColor.Execute then
    BackgroundColor := dlgColor.Color;
end;

end.
