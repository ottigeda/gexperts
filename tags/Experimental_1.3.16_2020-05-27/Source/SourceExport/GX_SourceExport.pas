unit GX_SourceExport;

// Original Author: ArentJan Banck <ajbanck@davilex.nl>

{$I GX_CondDefine.inc}

interface

uses
  Classes, Graphics, Controls, Forms, Dialogs, ActnList, Actions, ComCtrls,
  Menus, StdCtrls, ExtCtrls, ToolWin, UITypes,
  SynEdit, // This expert requires SynEdit from http://synedit.sf.net/
  GX_Experts, GX_ConfigurationInfo, GX_BaseForm;

type
  TGXCopyFormat = (cfText, cfHTMLFragment, cfRTFFragment);

  TSourceExportExpert = class;

  TfmSourceExport = class(TfmBaseForm)
    pnlFooter: TPanel;
    dlgSave: TSaveDialog;
    edtTitle: TEdit;
    lblTitle: TLabel;
    pmuCopy: TPopupMenu;
    mitCopyHtml: TMenuItem;
    mitCopyRtf: TMenuItem;
    mitCopy: TMenuItem;
    Actions: TActionList;
    actFileRefresh: TAction;
    actFileSave: TAction;
    actCopy: TAction;
    actCopyTextRtfHtml: TAction;
    actCopyHtmlFragment: TAction;
    actCopyRtfFragment: TAction;
    actFilePrint: TAction;
    actFileConfigure: TAction;
    actHelpHelp: TAction;
    actFileExit: TAction;
    pnlEditor: TPanel;
    pnlButtons: TPanel;
    pnlButtonsRight: TPanel;
    btnClose: TButton;
    btnConfig: TButton;
    btnPrint: TButton;
    btnCopy: TButton;
    btnSave: TButton;
    ToolBar: TToolBar;
    tbnRefresh: TToolButton;
    tbnSave: TToolButton;
    tbnCopy: TToolButton;
    ToolButton3: TToolButton;
    tbnPrint: TToolButton;
    ToolButton2: TToolButton;
    tbnConfigure: TToolButton;
    ToolButton1: TToolButton;
    tbnHelp: TToolButton;
    procedure actHelpExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure actRefreshExecute(Sender: TObject);
    procedure actPrintExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actConfigureExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actCopyHtmlExecute(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure actCopyRtfExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actCopyTextRtfHtmlExecute(Sender: TObject);
  private
    FEditor: TSynEdit;
    FHasBeenUsed: Boolean;
    procedure LoadSettings;
    procedure CopyToClipboard(CopyFormat: TGXCopyFormat);
    function FillEditControlWithIdeData: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    property HasBeenUsed: Boolean read FHasBeenUsed;
  end;

  TSourceExportExpert = class(TGX_Expert)
  private
    // Persistent configuration options
    FDefaultCopyFormat: TGXCopyFormat;
    FSaveDir: string;
    FSaveFilter: Integer;
    FBackgroundColor: TColor;
  protected
    procedure InternalLoadSettings(_Settings: IExpertSettings); override;
    procedure InternalSaveSettings(_Settings: IExpertSettings); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetActionCaption: string; override;
    class function GetName: string; override;
    procedure Configure; override;
    procedure Execute(Sender: TObject); override;
    procedure UpdateAction(Action: TCustomAction); override;
  end;

implementation

{$R *.dfm}

uses
  SysUtils, Windows, Clipbrd,
  SynEditExport, SynExportHtml, SynExportRtf, SynEditPrint,
  u_dzVclUtils,
  GX_GenericUtils, GX_GxUtils, GX_OtaUtils, GX_IdeUtils,
  GX_SynMemoUtils, GX_SourceExportOptions, GX_SharedImages;

const
  HighlighterDefaultRegKey = '\SourceExport\Highlighters\';

var
  SourceExportExpert: TSourceExportExpert = nil;

function TfmSourceExport.FillEditControlWithIdeData: Boolean;
var
  Lines: TGXUnicodeString;
begin
  Assert(Assigned(FEditor));

  FEditor.ClearAll;
  GxOtaGetEditorFont(FEditor.Font);

  SetSynEditHighlighter(FEditor, GetGXHighlighterForCurrentSourceEditor);

  Result := GxOtaGetActiveEditorTextAsUnicodeString(Lines);
  FEditor.Lines.Text := Lines;
end;

procedure TfmSourceExport.LoadSettings;
resourcestring
  SCopyText = 'Copy';
  SCopyHTML = 'Copy as HTML';
  SCopyRTF  = 'Copy as RTF';
var
  CaptionText: string;
begin
  Assert(Assigned(SourceExportExpert));
  Assert(Assigned(FEditor));

  FEditor.Highlighter.LoadFromRegistry(HKEY_CURRENT_USER, ConfigInfo.GExpertsIdeRootRegistryKey +
        HighlighterDefaultRegKey + FEditor.Highlighter.LanguageName);

  case SourceExportExpert.FDefaultCopyFormat of
    cfText:         CaptionText := SCopyText;
    cfHTMLFragment: CaptionText := SCopyHTML;
    cfRTFFragment:  CaptionText := SCopyRTF;
    else            Assert(False, 'Invalid TGXCopyFormat');
  end;

  btnCopy.Caption := '&' + CaptionText;
  tbnCopy.Hint := CaptionText;
  mitCopy.Default := True;
end;

procedure TfmSourceExport.FormActivate(Sender: TObject);
resourcestring
  SDialogFragmentExportTitle = 'Fragment of %s';
var
  CurrentModuleFileName: string;
  HasBlockSelection: Boolean;
begin
  CurrentModuleFileName := ExtractFileName(GxOtaGetTopMostEditBufferFileName);

  HasBlockSelection := FillEditControlWithIdeData;
  LoadSettings;
  if HasBlockSelection then
    edtTitle.Text := Format(SDialogFragmentExportTitle, [CurrentModuleFileName])
  else
    edtTitle.Text := CurrentModuleFileName;
end;

procedure TfmSourceExport.actRefreshExecute(Sender: TObject);
begin
  FillEditControlWithIdeData;
  LoadSettings;
end;

procedure TfmSourceExport.actSaveExecute(Sender: TObject);
resourcestring
  SDialogTitle = 'Save %s As';
var
  Exporter: TSynCustomExporter;
begin
  Assert(Assigned(SourceExportExpert));

  dlgSave.Title := Format(SDialogTitle, [edtTitle.Text]);
  dlgSave.FileName := ChangeFileExt(edtTitle.Text, '');

  dlgSave.InitialDir := SourceExportExpert.FSaveDir;
  dlgSave.FilterIndex := SourceExportExpert.FSaveFilter;

  if GetOpenSaveDialogExecute(dlgSave) then
  begin
    TCursor_TempHourglass;
    SourceExportExpert.FSaveDir := ExtractFilePath(ExpandFileName(dlgSave.FileName));
    SourceExportExpert.FSaveFilter := dlgSave.FilterIndex;
    if dlgSave.FilterIndex = 1 then
      Exporter := TSynExporterHTML.Create(nil)
    else
      Exporter := TSynExporterRTF.Create(nil);
    try
      Exporter.Title := ExtractFileName(dlgSave.FileName);
      Exporter.UseBackground := True;
      Exporter.Highlighter := FEditor.Highlighter;
      Exporter.ExportAsText := True;
      Exporter.Font := FEditor.Font;
      Exporter.Color := SourceExportExpert.FBackgroundColor;
      Application.ProcessMessages;
      Exporter.ExportAll(FEditor.Lines);
      Exporter.SaveToFile(dlgSave.FileName);
    finally
      FreeAndNil(Exporter);
    end;
    FHasBeenUsed := True;
  end;
end;

procedure TfmSourceExport.CopyToClipboard(CopyFormat: TGXCopyFormat);

  procedure ExportToClipboard(Exporter: TSynCustomExporter; AsText: Boolean);
  begin
    if Exporter = nil then
    begin
      Clipboard.AsText := FEditor.Lines.Text;
      Exit;
    end;
    if AsText and (Exporter is TSynExporterHTML) then
      (Exporter as TSynExporterHTML).CreateHTMLFragment := True;
    Exporter.Title := edtTitle.Text;
    Exporter.ExportAsText := AsText;
    Exporter.Highlighter := FEditor.Highlighter;
    Exporter.UseBackground := True;
    Exporter.ExportAll(FEditor.Lines);
    Exporter.CopyToClipboard;
  end;

var
  HtmlExporter: TSynCustomExporter;
  RtfExporter: TSynCustomExporter;
begin
  Assert(Assigned(FEditor));
  HtmlExporter := nil;
  RtfExporter := nil;
  try
    HtmlExporter := TSynExporterHTML.Create(nil);
    RtfExporter := TSynExporterRTF.Create(nil);

    case CopyFormat of
      cfText:
        begin
          Clipboard.Open;
          try
            ExportToClipboard(nil, False);
            ExportToClipboard(HtmlExporter, False);
            ExportToClipboard(RtfExporter, False);
          finally
            Clipboard.Close;
          end;
        end;
      cfHTMLFragment:
        ExportToClipboard(HtmlExporter, True);
      cfRTFFragment:
        ExportToClipboard(RtfExporter, True);
      else
        Assert(False, 'Unknown export type');
    end;
  finally
    FreeAndNil(HtmlExporter);
    FreeAndNil(RtfExporter);
  end;
end;

procedure TfmSourceExport.actCopyHtmlExecute(Sender: TObject);
begin
  CopyToClipboard(cfHTMLFragment);
end;

procedure TfmSourceExport.actCopyRtfExecute(Sender: TObject);
begin
  CopyToClipboard(cfRTFFragment);
end;

procedure TfmSourceExport.actCopyExecute(Sender: TObject);
begin
  Assert(Assigned(SourceExportExpert));
  CopyToClipboard(SourceExportExpert.FDefaultCopyFormat);
  FHasBeenUsed := True;
end;

procedure TfmSourceExport.actPrintExecute(Sender: TObject);
var
  SynPrint: TSynEditPrint;
begin
  Assert(Assigned(FEditor));
  Assert(Assigned(SourceExportExpert));
  TCursor_TempHourglass;
  SynPrint := TSynEditPrint.Create(nil);
  try
    SynPrint.SynEdit := FEditor;
    SynPrint.Highlight := (Assigned(FEditor.Highlighter));
    SynPrint.Colors := True;
    SynPrint.TabWidth := 4;
    SynPrint.Wrap := True;
    SynPrint.Title := edtTitle.Text;
    SynPrint.Print
  finally
    FreeAndNil(SynPrint);
  end;
  FHasBeenUsed := True;
end;

procedure TfmSourceExport.actConfigureExecute(Sender: TObject);
begin
  Assert(Assigned(SourceExportExpert));

  SourceExportExpert.Configure;
  LoadSettings;
end;

procedure TfmSourceExport.actHelpExecute(Sender: TObject);
begin
  GxContextHelp(Self, 26);
end;

procedure TfmSourceExport.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfmSourceExport.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Assert(Assigned(SourceExportExpert));
  SourceExportExpert.SaveSettings;
end;

procedure TfmSourceExport.actCopyTextRtfHtmlExecute(Sender: TObject);
begin
  CopyToClipboard(cfText); // This copies all registered clipboard formats at once
end;

constructor TfmSourceExport.Create(AOwner: TComponent);
begin
  inherited;

  TControl_SetMinConstraints(Self);

  SetToolbarGradient(ToolBar);
  // Destroyed with form
  FEditor := TSynEdit.Create(Self);
  with FEditor do
  begin
    Lines.Clear;
    Parent := pnlEditor;
    Align := alClient;
    TabOrder := 0;
    Gutter.Width := 0;
    TabWidth := 4;
    Options := Options - [eoScrollPastEof, eoScrollPastEol];
  end;

  dlgSave.Options := dlgSave.Options + [ofEnableSizing];
end;

{ TSourceExportExpert }

constructor TSourceExportExpert.Create;
begin
  inherited Create;

  SourceExportExpert := Self;
end;

procedure TSourceExportExpert.Execute(Sender: TObject);
var
  Dlg: TfmSourceExport;
begin
  Dlg := TfmSourceExport.Create(nil);
  try
    SetFormIcon(Dlg);
    Dlg.ShowModal;
    if Dlg.HasBeenUsed then
      IncCallCount;
  finally
    FreeAndNil(Dlg);
  end;
end;

procedure TSourceExportExpert.InternalLoadSettings(_Settings: IExpertSettings);
var
  NewCopyFormat: TGXCopyFormat;
begin
  inherited InternalLoadSettings(_Settings);
  // Do not localize.
  NewCopyFormat := TGXCopyFormat(_Settings.ReadEnumerated('Copy Format', TypeInfo(TGXCopyFormat), 0));
  Assert(NewCopyFormat in [Low(TGXCopyFormat)..High(TGXCopyFormat)]);
  FDefaultCopyFormat := NewCopyFormat;
  FSaveDir := _Settings.ReadString('Save Directory', '');
  FSaveFilter := _Settings.ReadInteger('Save Format', 1);
  FBackgroundColor := _Settings.ReadInteger('Background', GetIdeEditorBackgroundColor);
end;

procedure TSourceExportExpert.InternalSaveSettings(_Settings: IExpertSettings);
begin
  inherited InternalSaveSettings(_Settings);
  // Do not localize.
  _Settings.WriteInteger('Copy Format', Ord(FDefaultCopyFormat));
  _Settings.WriteString('Save Directory', FSaveDir);
  _Settings.WriteInteger('Save Format', FSaveFilter);
  _Settings.WriteInteger('Background', FBackgroundColor);
end;

procedure TSourceExportExpert.Configure;
var
  Dlg: TfmSourceExportOptions;
  HighlighterRegKey: string;
  NewCopyFormat: TGXCopyFormat;
begin
  Dlg := TfmSourceExportOptions.Create(nil);
  try
    HighlighterRegKey := ConfigInfo.GExpertsIdeRootRegistryKey + HighlighterDefaultRegKey
        + Dlg.SynSampleEditor.Highlighter.LanguageName;

    Dlg.rbxCopySettings.ItemIndex := Ord(FDefaultCopyFormat);
    Dlg.SynSampleEditor.Highlighter.LoadFromRegistry(HKEY_CURRENT_USER, HighlighterRegKey);
    Dlg.BackgroundColor := FBackgroundColor;
    if Dlg.ShowModal = mrOk then
    begin
      Dlg.SynSampleEditor.Highlighter.SaveToRegistry(HKEY_CURRENT_USER, HighlighterRegKey);

      NewCopyFormat := TGXCopyFormat(Dlg.rbxCopySettings.ItemIndex);
      Assert(NewCopyFormat in [Low(TGXCopyFormat)..High(TGXCopyFormat)]);
      FDefaultCopyFormat := NewCopyFormat;
      FBackgroundColor := Dlg.BackgroundColor;

      SaveSettings;
    end;
  finally
    FreeAndNil(Dlg);
  end;
end;

function TSourceExportExpert.GetActionCaption: string;
resourcestring
  SMenuCaption = '&Source Export...';
begin
  Result := SMenuCaption;
end;

class function TSourceExportExpert.GetName: string;
begin
  Result := 'SourceExport'; // Do not localize.
end;

destructor TSourceExportExpert.Destroy;
begin
  SourceExportExpert := nil;

  inherited Destroy;
end;

procedure TSourceExportExpert.UpdateAction(Action: TCustomAction);
const
  SAllowableFileExtensions = '.pas;.inc;.dpr;.txt;.cpp;.hpp;.c;.h;.sql;.htm;.html;.aspx';
begin
  Action.Enabled := FileMatchesExtensions(GxOtaGetCurrentSourceFile, SAllowableFileExtensions);
end;

initialization
  RegisterGX_Expert(TSourceExportExpert);

end.

