{Search history author: (ERT) Ferenc Kiffer, Hungary <kifferferenc@yahoo.com>}

unit GX_GrepResultsOptions;

{$I GX_CondDefine.inc}

interface

uses
  Windows, SysUtils, Classes, Graphics,
  StdCtrls, Dialogs, Controls, ExtCtrls, Forms, ComCtrls, GX_BaseForm;

type
  TPanel = class(ExtCtrls.TPanel)
  private
    ///<summary>
    /// Simple painting without theeming, so we can set the background color </summary>
    procedure CustomPaint;
  protected
    ///<summary>
    /// If Tag = 1 calls CustomPaint, otherwise calls inherited Paint </summary>
    procedure Paint; override;
  end;

type
  TfmGrepResultsOptions = class(TfmBaseForm)
    gbxMatchList: TGroupBox;
    pnlListFont: TPanel;
    gbxMatchContext: TGroupBox;
    pnlContextFont: TPanel;
    lblContextLines: TLabel;
    btnOK: TButton;
    btnCancel: TButton;
    pnlContextMatchFontColor: TPanel;
    chkGrepMiddle: TCheckBox;
    chkGrepExpandAll: TCheckBox;
    edtContextLines: TEdit;
    udContextLines: TUpDown;
    gbxHistoryList: TGroupBox;
    gbxListColors: TGroupBox;
    pnlListMatchTextColor: TPanel;
    chkDefaultListColors: TCheckBox;
    pnlListMatchBackgroundColor: TPanel;
    pnlContextMatchLineFontColor: TPanel;
    chkGrepAutoHide: TCheckBox;
    chkGrepSaveHistoryListItems: TCheckBox;
    chkGrepExpandIf: TCheckBox;
    eExpandIfMatches: TEdit;
    eExpandIfFiles: TEdit;
    lblExpandIfMatches: TLabel;
    lblExpandIfFiles: TLabel;
    chkGrepExpandFew: TCheckBox;
    lblExpandFewLines: TLabel;
    eExpandFewLines: TEdit;
    rbSaveToRegistry: TRadioButton;
    rbSaveToIniFile: TRadioButton;
    chkFileListDeleteAfterDays: TCheckBox;
    eDeleteAfterDays: TEdit;
    lblSaveOption: TLabel;
    cbxSearchSaveOptionDefaultValue: TComboBox;
    lblOnlySaveParamsAction: TLabel;
    cbxOnlySaveParamsAction: TComboBox;
    lblHistoryListDefaultPage: TLabel;
    cbxHistoryListDefaultPage: TComboBox;
    chkAdvanced: TCheckBox;
    chkQuickRefreshMode: TCheckBox;
    chkEmptyMoveToParams: TCheckBox;
    cbxOpenSaveOptionDefaultValue: TComboBox;
    lblSearchSaveOption: TLabel;
    lblOpenSaveOption: TLabel;
    chkHistoryPagesTabMultiLine: TCheckBox;
    eHistoryPagesTabWidth: TEdit;
    lblHistoryPagesTabWidth: TLabel;
    chkMouseWheelMoveItemIndex: TCheckBox;
    lblWhenIDEClosing: TLabel;
    procedure pnlContextFontClick(Sender: TObject);
    procedure pnlContextMatchFontColorClick(Sender: TObject);
    procedure pnlListFontClick(Sender: TObject);
    procedure pnlListMatchTextColorClick(Sender: TObject);
    procedure pnlListMatchBackgroundColorClick(Sender: TObject);
    procedure pnlContextMatchLineFontColorClick(Sender: TObject);
    procedure chkDefaultListColorsClick(Sender: TObject);
    procedure chkGrepExpandClick(Sender: TObject);
    procedure chkGrepSaveHistoryListItemsClick(Sender: TObject);
    procedure chkAdvancedClick(Sender: TObject);
    procedure chkFileListDeleteAfterDaysClick(Sender: TObject);
    procedure chkHistoryPagesTabMultiLineClick(Sender: TObject);
  private
    FListMatchTextColor: TColor;
    FListMatchBackgroundColor: TColor;
    FExpandsChanging: Boolean;
    function SelectColor(_Pnl: TPanel): Boolean;
    procedure SetData(
      _AdvancedOptions: Boolean;
      _Middle: Boolean;
      _ExpandAll: Boolean;
      _ExpandIf: Boolean;
      _ExpandIfFiles: Integer;
      _ExpandIfMatches: Integer;
      _ExpandFew: Boolean;
      _ExpandFewLines: Integer;
      _UseDefaultColors: Boolean;
      _ListFont: TFont;
      _ListMatchTextColor: TColor;
      _ListMatchBrushColor: TColor;
      _ContextFont: TFont;
      _ContextMatchLineColor: TColor;
      _ContextMatchColor: TColor;
      _NumContextLines: Integer;
      _SaveHistoryListItems: integer;
      _AutoHide: Boolean;
      _FileListDeleteAfterDays: Boolean;
      _DeleteAfterDays: Integer;
      _EmptyMoveToOnlySaveParams: Boolean;
      _SaveOptionDefaultValue: Integer;
      _OpenSaveOptionDefaultValue: Integer;
      _OnlySaveParamsAction: Integer;
      _HistoryListDefaultPage: Integer;
      _QuickRefresh: Boolean;
      _HistoryPagesTabMultiline: Boolean;
      _HistoryPagesTabWidth: Integer;
      _MouseWheelPrevNextMatch: Boolean);
    procedure GetData(out _AdvancedOptions: Boolean;
      out _Middle: Boolean;
      out _ExpandAll: Boolean;
      out _ExpandIf: Boolean;
      out _ExpandIfFiles: Integer;
      out _ExpandIfMatches: Integer;
      out _ExpandFew: Boolean;
      out _ExpandFewLines: Integer;
      out _ListUseDefaultColors: Boolean;
      _ListFont: TFont;
      out _ListMatchTextColor: TColor;
      out _ListMatchBrushColor: TColor;
      _ContextFont: TFont;
      out _ContextMatchLineColor: TColor;
      out _ContextMatchColor: TColor;
      out _NumContextLines: Integer;
      out _SaveHistoryListItems: Integer;
      out _AutoHide: Boolean;
      out _FileListDeleteAfterDays: Boolean;
      out _DeleteAfterDays: Integer;
      out _EmptyMoveToOnlySaveParams: Boolean;
      out _SaveOptionDefaultValue: Integer;
      out _OpenSaveOptionDefaultValue: Integer;
      out _OnlySaveParamsAction: Integer;
      out _HistoryListDefaultPage: Integer;
      out _QuickRefresh: Boolean;
      out _HistoryPagesTabMultiline: Boolean;
      out _HistoryPagesTabWidth: Integer;
      out _MouseWheelPrevNextMatch: Boolean);
    procedure SelectFont(_Pnl: TPanel);
  public
    constructor Create(_Owner: TComponent); override;
    class function Execute(_Owner: TWinControl;
      var _AdvancedOptions: Boolean;
      var _Middle: Boolean;
      var _ExpandAll: Boolean;
      var _ExpandIf: Boolean;
      var _ExpandIfFiles: Integer;
      var _ExpandIfMatches: Integer;
      var _ExpandFew: Boolean;
      var _ExpandFewLines: Integer;
      var _ListUseDefaultColors: Boolean;
      _ListFont: TFont;
      var _ListMatchTextColor: TColor;
      var _ListMatchBrushColor: TColor;
      _ContextFont: TFont;
      var _ContextMatchLineColor: TColor;
      var _ContextMatchColor: TColor;
      var _NumContextLines: Integer;
      var _SaveHistoryListItems: integer;
      var _AutoHide: Boolean;
      var _FileListDeleteAfterDays: Boolean;
      var _DeleteAfterDays: Integer;
      var _EmptyMoveToOnlySaveParams: Boolean;
      var _SaveOptionDefaultValue: Integer;
      var _OpenSaveOptionDefaultValue: Integer;
      var _OnlySaveParamsAction: Integer;
      var _HistoryListDefaultPage: Integer;
      var _QuickRefresh: Boolean;
      var _HistoryPagesTabMultiline: Boolean;
      var _HistoryPagesTabWidth: Integer;
      var _MouseWheelPrevNextMatch: Boolean): Boolean;
  end;

implementation

{$R *.dfm}

uses
  u_dzVclUtils, u_dzGraphicsUtils;

class function TfmGrepResultsOptions.Execute(_Owner: TWinControl;
  var _AdvancedOptions: Boolean;
  var _Middle: Boolean;
  var _ExpandAll: Boolean;
  var _ExpandIf: Boolean;
  var _ExpandIfFiles: Integer;
  var _ExpandIfMatches: Integer;
  var _ExpandFew: Boolean;
  var _ExpandFewLines: Integer;
  var _ListUseDefaultColors: Boolean;
  _ListFont: TFont;
  var _ListMatchTextColor: TColor;
  var _ListMatchBrushColor: TColor;
  _ContextFont: TFont;
  var _ContextMatchLineColor: TColor;
  var _ContextMatchColor: TColor;
  var _NumContextLines: Integer;
  var _SaveHistoryListItems: Integer;
  var _AutoHide: Boolean;
  var _FileListDeleteAfterDays: Boolean;
  var _DeleteAfterDays: Integer;
  var _EmptyMoveToOnlySaveParams: Boolean;
  var _SaveOptionDefaultValue: Integer;
  var _OpenSaveOptionDefaultValue: Integer;
  var _OnlySaveParamsAction: Integer;
  var _HistoryListDefaultPage: Integer;
  var _QuickRefresh: Boolean;
  var _HistoryPagesTabMultiline: Boolean;
  var _HistoryPagesTabWidth: Integer;
  var _MouseWheelPrevNextMatch: Boolean): Boolean;
var
  frm: TfmGrepResultsOptions;
begin
  frm := TfmGrepResultsOptions.Create(_Owner);
  try
    TForm_CenterOn(frm, _Owner);
    frm.SetData(_AdvancedOptions, _Middle,
      _ExpandAll, _ExpandIf, _ExpandIfFiles, _ExpandIfMatches, _ExpandFew, _ExpandFewLines,
      _ListUseDefaultColors, _ListFont, _ListMatchTextColor, _ListMatchBrushColor,
      _ContextFont, _ContextMatchLineColor, _ContextMatchColor, _NumContextLines,
      _SaveHistoryListItems,
      _AutoHide, _FileListDeleteAfterDays, _DeleteAfterDays,
      _EmptyMoveToOnlySaveParams, _SaveOptionDefaultValue, _OpenSaveOptionDefaultValue,
      _OnlySaveParamsAction, _HistoryListDefaultPage, _QuickRefresh, _HistoryPagesTabMultiline,
      _HistoryPagesTabWidth, _MouseWheelPrevNextMatch);
    Result := (frm.ShowModal = mrOk);
    if Result then
      frm.GetData(_AdvancedOptions, _Middle,
        _ExpandAll, _ExpandIf, _ExpandIfFiles, _ExpandIfMatches, _ExpandFew, _ExpandFewLines,
        _ListUseDefaultColors, _ListFont, _ListMatchTextColor, _ListMatchBrushColor,
        _ContextFont, _ContextMatchLineColor, _ContextMatchColor, _NumContextLines,
        _SaveHistoryListItems,
        _AutoHide, _FileListDeleteAfterDays, _DeleteAfterDays,
        _EmptyMoveToOnlySaveParams, _SaveOptionDefaultValue, _OpenSaveOptionDefaultValue,
        _OnlySaveParamsAction, _HistoryListDefaultPage, _QuickRefresh, _HistoryPagesTabMultiline,
        _HistoryPagesTabWidth, _MouseWheelPrevNextMatch);
  finally
    frm.Free;
  end;
end;

constructor TfmGrepResultsOptions.Create(_Owner: TComponent);
begin
  inherited;

  TControl_SetMinConstraints(Self);

  // Tag = 1 is used a a flag for custom painting.
  // If we drop support for older Delphi versions we could simply set the StyleElements property
  // to [seFont] and get rid of the TPanel interceptor class.
  pnlListMatchTextColor.Tag := 1;
  pnlListMatchBackgroundColor.Tag := 1;
  pnlContextMatchFontColor.Tag := 1;
  pnlContextMatchLineFontColor.Tag := 1;

  InitDpiScaler;
end;

procedure TfmGrepResultsOptions.SetData(
  _AdvancedOptions: Boolean;
  _Middle: Boolean;
  _ExpandAll: Boolean;
  _ExpandIf: Boolean;
  _ExpandIfFiles: Integer;
  _ExpandIfMatches: Integer;
  _ExpandFew: Boolean;
  _ExpandFewLines: Integer;
  _UseDefaultColors: Boolean;
  _ListFont: TFont;
  _ListMatchTextColor: TColor;
  _ListMatchBrushColor: TColor;
  _ContextFont: TFont;
  _ContextMatchLineColor: TColor;
  _ContextMatchColor: TColor;
  _NumContextLines: Integer;
  _SaveHistoryListItems: Integer;
  _AutoHide: Boolean;
  _FileListDeleteAfterDays: Boolean;
  _DeleteAfterDays: Integer;
  _EmptyMoveToOnlySaveParams: Boolean;
  _SaveOptionDefaultValue: Integer;
  _OpenSaveOptionDefaultValue: Integer;
  _OnlySaveParamsAction: Integer;
  _HistoryListDefaultPage: Integer;
  _QuickRefresh: Boolean;
  _HistoryPagesTabMultiline: Boolean;
  _HistoryPagesTabWidth: Integer;
  _MouseWheelPrevNextMatch: Boolean);
begin
  chkAdvanced.Checked := _AdvancedOptions;
  chkAdvancedClick(nil);

  chkGrepMiddle.Checked := _Middle;
  chkGrepExpandAll.Checked := _ExpandAll;
  chkGrepExpandIf.Checked := _ExpandIf;
  eExpandIfFiles.Text := IntToStr(_ExpandIfFiles);
  eExpandIfMatches.Text := IntToStr(_ExpandIfMatches);
  chkGrepExpandFew.Checked := _ExpandFew;
  eExpandFewLines.Text := IntToStr(_ExpandFewLines);

  FListMatchTextColor := _ListMatchTextColor;
  FListMatchBackgroundColor := _ListMatchBrushColor;
  pnlListFont.Font.Assign(_ListFont);
  pnlListMatchTextColor.Color := _ListMatchTextColor;
  pnlListMatchTextColor.Font.Color := BestForegroundForColor(_ListMatchTextColor);
  pnlListMatchBackgroundColor.Color := _ListMatchBrushColor;
  pnlListMatchBackgroundColor.Font.Color := BestForegroundForColor(_ListMatchBrushColor);
  chkDefaultListColors.Checked := _UseDefaultColors;

  pnlContextFont.Font.Assign(_ContextFont);
  pnlContextMatchLineFontColor.Color := _ContextMatchLineColor;
  pnlContextMatchLineFontColor.Font.Color := BestForegroundForColor(_ContextMatchLineColor);
  pnlContextMatchFontColor.Color := _ContextMatchColor;
  pnlContextMatchFontColor.Font.Color := BestForegroundForColor(_ContextMatchColor);

  udContextLines.Position := _NumContextLines;
  chkGrepSaveHistoryListItems.Checked := (_SaveHistoryListItems in [1, 2]);
  rbSaveToIniFile.Checked := (_SaveHistoryListItems = 1);
  rbSaveToRegistry.Checked := (_SaveHistoryListItems = 2);

  chkGrepAutoHide.Checked := _AutoHide;

  chkFileListDeleteAfterDays.Checked := _FileListDeleteAfterDays;
  eDeleteAfterDays.Text := IntToStr(_DeleteAfterDays);
  chkEmptyMoveToParams.Checked := _EmptyMoveToOnlySaveParams;
  cbxSearchSaveOptionDefaultValue.ItemIndex := _SaveOptionDefaultValue;
  cbxOpenSaveOptionDefaultValue.ItemIndex := _OpenSaveOptionDefaultValue;
  cbxOnlySaveParamsAction.ItemIndex := _OnlySaveParamsAction;
  cbxHistoryListDefaultPage.ItemIndex := _HistoryListDefaultPage;
  chkQuickRefreshMode.Checked := _QuickRefresh;

  chkHistoryPagesTabMultiLine.Checked := _HistoryPagesTabMultiline;
  eHistoryPagesTabWidth.Text := IntToStr(_HistoryPagesTabWidth);
  chkMouseWheelMoveItemIndex.Checked := _MouseWheelPrevNextMatch;
end;

procedure TfmGrepResultsOptions.GetData(out _AdvancedOptions: Boolean;
  out _Middle: Boolean;
  out _ExpandAll: Boolean;
  out _ExpandIf: Boolean;
  out _ExpandIfFiles: Integer;
  out _ExpandIfMatches: Integer;
  out _ExpandFew: Boolean;
  out _ExpandFewLines: Integer;
  out _ListUseDefaultColors: Boolean;
  _ListFont: TFont;
  out _ListMatchTextColor: TColor;
  out _ListMatchBrushColor: TColor;
  _ContextFont: TFont;
  out _ContextMatchLineColor: TColor;
  out _ContextMatchColor: TColor;
  out _NumContextLines: Integer;
  out _SaveHistoryListItems: Integer;
  out _AutoHide: Boolean;
  out _FileListDeleteAfterDays: Boolean;
  out _DeleteAfterDays: Integer;
  out _EmptyMoveToOnlySaveParams: Boolean;
  out _SaveOptionDefaultValue: Integer;
  out _OpenSaveOptionDefaultValue: Integer;
  out _OnlySaveParamsAction: Integer;
  out _HistoryListDefaultPage: Integer;
  out _QuickRefresh: Boolean;
  out _HistoryPagesTabMultiline: Boolean;
  out _HistoryPagesTabWidth: Integer;
  out _MouseWheelPrevNextMatch: Boolean);
begin
  _AdvancedOptions := chkAdvanced.Checked;

  _Middle := chkGrepMiddle.Checked;
  _ExpandAll := chkGrepExpandAll.Checked;
  _ExpandIf := _AdvancedOptions and chkGrepExpandIf.Checked;
  _ExpandIfFiles := StrToIntDef(eExpandIfFiles.Text, 25);
  _ExpandIfMatches := StrToIntDef(eExpandIfMatches.Text, 150);
  _ExpandFew := _AdvancedOptions and chkGrepExpandFew.Checked;
  _ExpandFewLines := StrToIntDef(eExpandFewLines.Text, 20);

  _ListFont.Assign(pnlListFont.Font);
  _ListUseDefaultColors := chkDefaultListColors.Checked;
  if _ListUseDefaultColors then begin
    _ListMatchTextColor := FListMatchTextColor;
    _ListMatchBrushColor := FListMatchBackgroundColor;
  end else begin
    _ListMatchTextColor := pnlListMatchTextColor.Color;
    _ListMatchBrushColor := pnlListMatchBackgroundColor.Color;
  end;

  _ContextFont.Assign(pnlContextFont.Font);
  _ContextMatchLineColor := pnlContextMatchLineFontColor.Color;
  _ContextMatchColor := pnlContextMatchFontColor.Color;

  _NumContextLines := udContextLines.Position;
  if _AdvancedOptions then begin
    if not chkGrepSaveHistoryListItems.Checked then
      _SaveHistoryListItems := 0
    else if rbSaveToIniFile.Checked then
      _SaveHistoryListItems := 1
    else if rbSaveToRegistry.Checked then
      _SaveHistoryListItems := 2;
  end else begin
    if not chkGrepSaveHistoryListItems.Checked then
      _SaveHistoryListItems := 0
    else
      _SaveHistoryListItems := 1;
  end;

  _FileListDeleteAfterDays := chkFileListDeleteAfterDays.Checked;
  _DeleteAfterDays := StrToIntDef(eDeleteAfterDays.Text, 30);
  _SaveOptionDefaultValue := cbxSearchSaveOptionDefaultValue.ItemIndex;
  _OpenSaveOptionDefaultValue := cbxOpenSaveOptionDefaultValue.ItemIndex;
  _EmptyMoveToOnlySaveParams := _AdvancedOptions and chkEmptyMoveToParams.Checked;
  _OnlySaveParamsAction := cbxOnlySaveParamsAction.ItemIndex;
  _HistoryListDefaultPage := cbxHistoryListDefaultPage.ItemIndex;
  _QuickRefresh := chkQuickRefreshMode.Checked;

  if _AdvancedOptions then begin
    _HistoryPagesTabMultiline := chkHistoryPagesTabMultiLine.Checked;
    _HistoryPagesTabWidth := StrToIntDef(eHistoryPagesTabWidth.Text, _HistoryPagesTabWidth);
  end;

  _MouseWheelPrevNextMatch := _AdvancedOptions and chkMouseWheelMoveItemIndex.Checked;

  _AutoHide := chkGrepAutoHide.Checked;
end;

procedure TfmGrepResultsOptions.chkAdvancedClick(Sender: TObject);
var
  IsAdvanced: Boolean;
begin
  IsAdvanced := chkAdvanced.Checked;
  rbSaveToRegistry.Visible := IsAdvanced;
  rbSaveToIniFile.Visible := IsAdvanced;
  chkEmptyMoveToParams.Visible := IsAdvanced;
  chkGrepExpandIf.Visible := IsAdvanced;
  chkGrepExpandFew.Visible := IsAdvanced;
  lblExpandIfMatches.Visible := IsAdvanced;
  lblExpandIfFiles.Visible := IsAdvanced;
  lblExpandFewLines.Visible := IsAdvanced;
  eExpandIfMatches.Visible := IsAdvanced;
  eExpandIfFiles.Visible := IsAdvanced;
  eExpandFewLines.Visible := IsAdvanced;
  chkHistoryPagesTabMultiLine.Visible := IsAdvanced;
  lblHistoryPagesTabWidth.Visible := IsAdvanced;
  eHistoryPagesTabWidth.Visible := IsAdvanced;
  chkMouseWheelMoveItemIndex.Visible := IsAdvanced;

  chkQuickRefreshMode.Visible := IsAdvanced;
end;

procedure TfmGrepResultsOptions.pnlListFontClick(Sender: TObject);
begin
  SelectFont(pnlListFont);
end;

procedure TfmGrepResultsOptions.SelectFont(_Pnl: TPanel);
var
  PrevColor: TColor;
  dlg: TFontDialog;
begin
  dlg := TFontDialog.Create(Self);
  try
    dlg.Name := '';
    PrevColor := _Pnl.Font.Color;
    dlg.Font.Assign(_Pnl.Font);
    if dlg.Execute then begin
      _Pnl.Font.Assign(dlg.Font);
      _Pnl.Font.Color := PrevColor;
      _Pnl.Refresh;
    end;
  finally
    dlg.Free;
  end;
end;

function TfmGrepResultsOptions.SelectColor(_Pnl: TPanel): Boolean;
var
  dlg: TColorDialog;
begin
  dlg := TColorDialog.Create(Self);
  try
    dlg.Name := '';
    dlg.Options := [cdSolidColor];
    dlg.Color := _Pnl.Color;
    Result := dlg.Execute;
    if Result then begin
      _Pnl.Color := dlg.Color;
      _Pnl.Font.Color := BestForegroundForColor(_Pnl.Color)
    end;
  finally
    dlg.Free;
  end;
end;

procedure TfmGrepResultsOptions.pnlListMatchTextColorClick(Sender: TObject);
begin
  SelectColor(pnlListMatchTextColor);
end;

procedure TfmGrepResultsOptions.pnlListMatchBackgroundColorClick(Sender: TObject);
begin
  SelectColor(pnlListMatchBackgroundColor);
end;

procedure TfmGrepResultsOptions.pnlContextFontClick(Sender: TObject);
begin
  SelectFont(pnlContextFont);
end;

procedure TfmGrepResultsOptions.pnlContextMatchLineFontColorClick(Sender: TObject);
begin
  SelectColor(pnlContextMatchLineFontColor);
end;

procedure TfmGrepResultsOptions.pnlContextMatchFontColorClick(Sender: TObject);
begin
  SelectColor(pnlContextMatchFontColor);
end;

procedure TfmGrepResultsOptions.chkDefaultListColorsClick(Sender: TObject);
begin
  if chkDefaultListColors.Checked then begin
    // save colors in case the user wants to use them again
    FListMatchTextColor := pnlListMatchTextColor.Color;
    FListMatchBackgroundColor := pnlListMatchBackgroundColor.Color;
    pnlListMatchTextColor.Enabled := False;
    pnlListMatchTextColor.ParentColor := True;
    pnlListMatchTextColor.Font.Color := clGrayText;

    pnlListMatchBackgroundColor.Enabled := False;
    pnlListMatchBackgroundColor.ParentColor := True;
    pnlListMatchBackgroundColor.Font.Color := clGrayText;
  end else begin
    pnlListMatchTextColor.Enabled := True;
    // restore colors that were saved before
    pnlListMatchTextColor.Color := FListMatchTextColor;
    pnlListMatchTextColor.Font.Color := BestForegroundForColor(FListMatchTextColor);
    pnlListMatchBackgroundColor.Enabled := True;
    pnlListMatchBackgroundColor.Color := FListMatchBackgroundColor;
    pnlListMatchBackgroundColor.Font.Color := BestForegroundForColor(FListMatchBackgroundColor);
  end;
end;

procedure TfmGrepResultsOptions.chkGrepExpandClick(Sender: TObject);
begin
  if FExpandsChanging then
    Exit;

  FExpandsChanging := True;
  try
    chkGrepExpandAll.Checked := Sender = chkGrepExpandAll;
    chkGrepExpandIf.Checked := Sender = chkGrepExpandIf;
    chkGrepExpandFew.Checked := Sender = chkGrepExpandFew;

    lblExpandIfMatches.Enabled := chkGrepExpandIf.Checked;
    eExpandIfMatches.Enabled := chkGrepExpandIf.Checked;
    lblExpandIfFiles.Enabled := chkGrepExpandIf.Checked;
    eExpandIfFiles.Enabled := chkGrepExpandIf.Checked;

    lblExpandFewLines.Enabled := chkGrepExpandFew.Checked;
    eExpandFewLines.Enabled := chkGrepExpandFew.Checked;
  finally
    FExpandsChanging := False;
  end;
end;

procedure TfmGrepResultsOptions.chkGrepSaveHistoryListItemsClick(Sender: TObject);
begin
  rbSaveToIniFile.Enabled := chkGrepSaveHistoryListItems.Checked;
  rbSaveToRegistry.Enabled := chkGrepSaveHistoryListItems.Checked;
end;

procedure TfmGrepResultsOptions.chkHistoryPagesTabMultiLineClick(Sender: TObject);
begin
  eHistoryPagesTabWidth.Enabled := not chkHistoryPagesTabMultiLine.Checked;
end;

procedure TfmGrepResultsOptions.chkFileListDeleteAfterDaysClick(Sender: TObject);
begin
  eDeleteAfterDays.Enabled := chkFileListDeleteAfterDays.Checked;
end;

procedure TPanel.CustomPaint;

  procedure DrawBevel(_Bevel: TPanelBevel; _Rect: TRect);
  var
    TopColor: TColor;
    BottomColor: TColor;
  begin
    TopColor := clBtnHighlight;
    if _Bevel = bvLowered then
      TopColor := clBtnShadow;
    BottomColor := clBtnShadow;
    if _Bevel = bvLowered then
      BottomColor := clBtnHighlight;
    Frame3D(Canvas, _Rect, TopColor, BottomColor, BevelWidth);
  end;

const
  Alignments: array[TAlignment] of LongWord = (DT_LEFT, DT_RIGHT, DT_CENTER);
var
  Rect: TRect;
  FontHeight: Integer;
  Flags: LongWord;
begin
  Rect := GetClientRect;
  if BevelOuter <> bvNone then
    DrawBevel(BevelOuter, Rect);
  Frame3D(Canvas, Rect, Color, Color, BorderWidth);
  if BevelInner <> bvNone then
    DrawBevel(BevelInner, Rect);

  Canvas.Brush.Color := Color;
  Canvas.FillRect(Rect);

  FontHeight := Canvas.TextHeight('W');
{$IFDEF GX_IDE_IS_HIDPI_AWARE}
  FontHeight := MulDiv(FontHeight, GetParentForm(Self).CurrentPPI, GetDesignDpi);
{$ENDIF}
  Rect.Top := ((Rect.Bottom + Rect.Top) - FontHeight) div 2;
  Rect.Bottom := Rect.Top + FontHeight;
  Canvas.Brush.Style := bsClear;
  Canvas.Font := Self.Font;
  Flags := DT_EXPANDTABS or DT_VCENTER or Alignments[Self.Alignment];
  Flags := DrawTextBiDiModeFlags(Flags);
  DrawText(Canvas.Handle, PChar(Self.Caption), -1, Rect, Flags);
end;

procedure TPanel.Paint;
begin
  if Tag = 1 then
    CustomPaint
  else
    inherited;
end;

end.
