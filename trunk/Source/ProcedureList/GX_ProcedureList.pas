unit GX_ProcedureList;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils, Classes, ActnList, Actions, Dialogs, ComCtrls, ToolWin, StdCtrls,
  Controls, ExtCtrls, Messages, Forms, GX_EnhancedEditor,
  GX_ProcedureListOptions, GX_FileScanner, GX_EditReader, GX_BaseForm;

const
  UM_RESIZECOLS = WM_USER + 523;

type
  TfmProcedureList = class(TfmBaseForm)
    pnlFuncHolder: TPanel;
    pnHolder: TPanel;
    lvProcs: TListView;
    StatusBar: TStatusBar;
    pnlHeader: TPanel;
    dlgProcFont: TFontDialog;
    pnlHeaderLeft: TPanel;
    lblMethods: TLabel;
    edtMethods: TEdit;
    pnlHeaderRight: TPanel;
    cbxObjects: TComboBox;
    lblObjects: TLabel;
    Actions: TActionList;
    ToolBar: TToolBar;
    tbnCopy: TToolButton;
    tbnSep1: TToolButton;
    tbnStart: TToolButton;
    tbnAny: TToolButton;
    tbnSep3: TToolButton;
    tbnGoto: TToolButton;
    tbnSep4: TToolButton;
    tbnHelp: TToolButton;
    actEditCopy: TAction;
    actOptionsFont: TAction;
    actViewStart: TAction;
    actViewAny: TAction;
    actViewGoto: TAction;
    actHelpHelp: TAction;
    tbnShowFunctionCode: TToolButton;
    pnlFunctionBody: TPanel;
    splSeparator: TSplitter;
    tbnOptions: TToolButton;
    actOptions: TAction;
    tbnSep5: TToolButton;
    tbnSep6: TToolButton;
    tmrFilter: TTimer;
    tbnMatchClass: TToolButton;
    tbnSep2: TToolButton;
    tbnMatchProc: TToolButton;
    actMatchClass: TAction;
    actMatchMethod: TAction;
    procedure actMatchMethodExecute(Sender: TObject);
    procedure actMatchClassExecute(Sender: TObject);
    procedure tmrFilterTimer(Sender: TObject);
    procedure lvProcsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure lvProcsColumnClick(Sender: TObject; Column: TListColumn);
    procedure edtMethodsChange(Sender: TObject);
    procedure edtMethodsKeyPress(Sender: TObject; var Key: Char);
    procedure edtMethodsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure cbxObjectsChange(Sender: TObject);
    procedure pnlHeaderResize(Sender: TObject);
    procedure actEditCopyExecute(Sender: TObject);
    procedure actHelpHelpExecute(Sender: TObject);
    procedure actOptionsFontExecute(Sender: TObject);
    procedure actViewStartExecute(Sender: TObject);
    procedure actViewAnyExecute(Sender: TObject);
    procedure actViewGotoExecute(Sender: TObject);
    procedure ActionsUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure tbnShowFunctionCodeClick(Sender: TObject);
    procedure actOptionsExecute(Sender: TObject);
    procedure lvProcsCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
    procedure splSeparatorCanResize(Sender: TObject; var NewSize: Integer; var Accept: Boolean);
    procedure lvProcsResize(Sender: TObject);
  private
    FFileScanner: TFileScanner;
    FEditReader: TEditReader;
    FFileName: string;
    FObjectStrings: TStringList;
    FLanguage: TSourceLanguage;
    FUnitText: string;
    FCodeText: TGXEnhancedEditor;
    FOptions: TProcedureListOptions;
    FLastProcLineNo: Integer;
    FMinListWidth: Integer;
    FMinListHeight: Integer;
    function GetImageIndex(const ProcName, ProcClass: string): Integer;
    procedure LoadProcs;
    procedure FillListBox;
    procedure ResizeCols;
    procedure GotoCurrentlySelectedProcedure;
    procedure UMResizeCols(var Msg: TMessage); message UM_RESIZECOLS;
    procedure ClearObjectStrings;
    procedure LoadObjectCombobox;
    procedure InitializeForm;
    procedure LoadSettings;
    procedure SaveSettings;
    procedure SetupSyntaxHighlightingControl;
    procedure ApplyOptions(const bLoading: Boolean);
    procedure UpdateCodeView(ProcInfo: TProcedure);
    function CurrentProcInfo: TProcedure;
  public
    constructor CreateWithFileName(AOwner: TComponent; const FileName: string);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Language: TSourceLanguage read FLanguage write FLanguage;
  end;

implementation

{$R *.dfm}

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  Windows, Clipbrd, Menus, StrUtils,
  GX_GxUtils, GX_GenericUtils, GX_OtaUtils, GX_IdeUtils,
  GX_SharedImages, GX_Experts, Math, GX_dzVclUtils;

resourcestring
  SAllString = '<All>';
  SNoneString = '<None>';

type
  TProcedureListExpert = class(TGX_Expert)
  public
    constructor Create; override;
    function GetActionCaption: string; override;
    function GetDefaultShortCut: TShortCut; override;
    class function GetName: string; override;
    procedure Execute(Sender: TObject); override;
    function HasConfigOptions: Boolean; override;
    function HasMenuItem: Boolean; override;
    procedure Configure; override;
    procedure UpdateAction(Action: TCustomAction); override;
  end;

constructor TfmProcedureList.CreateWithFileName(AOwner: TComponent; const FileName: string);
resourcestring
  SParseStatistics = 'Procedures processed in %g seconds';
var
  LoadTime: DWORD;
begin
  inherited Create(AOwner);
  TControl_SetMinConstraints(Self);

  FMinListWidth := 500;
  FMinListHeight := 200;

  SetNonModalFormPopupMode(Self);

  FFileName := FileName;
  FFileScanner := TFileScanner.CreateWithFileName(Self, FileName);
  FLastProcLineNo := -1;

  LoadTime := GetTickCount;
  InitializeForm;
  LoadTime := GetTickCount - LoadTime;
  StatusBar.Panels[0].Text := Format(SParseStatistics, [LoadTime / 1000]);
end;

procedure TfmProcedureList.LoadProcs;
begin
  FUnitText :=  TEditReader.GetText(FFileName);
  FFileScanner.UnitText := FUnitText;
  Caption := Caption + ' - ' + ExtractFileName(FFileName);

  ClearObjectStrings;
  try
    FFileScanner.Execute;
  finally
    LoadObjectCombobox;
  end;
  StatusBar.Panels[1].Text := Trim(IntToStr(lvProcs.Items.Count));
end;

function TfmProcedureList.GetImageIndex(const ProcName, ProcClass: string): Integer;
begin
  if StrContains('constructor', ProcName, False) then // Do not localize.
    Result := ImageIndexNew
  else if StrContains('destructor', ProcName, False) then // Do not localize.
    Result := ImageIndexTrash
  else if StartsText('class proc', ProcName) or StrContains('class func', ProcName, False) or (ProcClass <> '') then // Do not localize
    Result := ImageIndexGear
  else
    Result := ImageIndexFunction;
end;

procedure TfmProcedureList.lvProcsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
var
  ProcInfo: TProcedure;
begin
  ProcInfo := nil;
  if lvProcs.Selected <> nil then
    ProcInfo := lvProcs.Selected.Data;
  if ProcInfo <> nil then
  begin
    StatusBar.Panels[0].Text := ProcInfo.ProcLine;
    StatusBar.Panels[1].Text := Format('%d/%d', [lvProcs.Selected.Index + 1, lvProcs.Items.Count]);
    actViewGoto.Enabled := (lvProcs.Selected <> nil);
  end;
  if (Item <> nil) and Item.Selected then
    UpdateCodeView(ProcInfo);
end;

procedure TfmProcedureList.FillListBox;
var
  i: Integer;
  ProcInfo: TProcedure;
  IsObject: Boolean;

  procedure AddListItem(ProcInfo: TProcedure);
  var
    ListItem: TListItem;
  begin
    ListItem := lvProcs.Items.Add;
    ListItem.Caption := '';
    if FOptions.ObjectNameVisible then
    begin
      if ProcInfo.ProcClass <> '' then
        ListItem.SubItems.Add(ProcInfo.ProcClass + ProcInfo.ObjectSeparator + ProcInfo.ProcName)
      else
        ListItem.SubItems.Add(ProcInfo.ProcName)
    end
    else
      ListItem.SubItems.Add(ProcInfo.ProcName);
    ListItem.SubItems.Add(ProcInfo.ProcedureType);
    ListItem.SubItems.Add(IntToStr(ProcInfo.LineNo));
    ListItem.ImageIndex := GetImageIndex(ProcInfo.ProcedureType, ProcInfo.ProcClass);
    ListItem.Data := ProcInfo;
  end;

  procedure FocusAndSelectFirstItem;
  begin
    if lvProcs.Items.Count > 0 then
    begin
      lvProcs.Selected := lvProcs.Items[0];
      lvProcs.ItemFocused := lvProcs.Selected;
    end;
  end;

begin
  lvProcs.Items.BeginUpdate;
  try
    lvProcs.Items.Clear;
    if (Length(edtMethods.Text) = 0) and (cbxObjects.Text = SAllString) then
    begin
      for i := 0 to FFileScanner.Procedures.Count - 1 do
        AddListItem(FFileScanner.Procedures.Items[i]);
      Exit;
    end;

    for i := 0 to FFileScanner.Procedures.Count - 1 do
    begin
      ProcInfo := FFileScanner.Procedures.Items[i];
      IsObject := Length(ProcInfo.ProcClass) > 0;

      // Is it the object we want?
      if (cbxObjects.Text = SNoneString) then
      begin
        if IsObject then
          Continue
      end
      else if (cbxObjects.Text <> SAllString) and
        (not SameText(cbxObjects.Text, ProcInfo.ProcClass)) then
        Continue;

      if Length(edtMethods.Text) = 0 then
        AddListItem(ProcInfo)
      else if not FOptions.SearchAll and StartsText(edtMethods.Text, ProcInfo.ProcName) then
        AddListItem(ProcInfo)
      else if not FOptions.SearchAll and FOptions.SearchClassName and StartsText(edtMethods.Text, ProcInfo.ProcClass) then
        AddListItem(ProcInfo)
      else if FOptions.SearchAll and StrContains(edtMethods.Text, ProcInfo.ProcName, False) then
        AddListItem(ProcInfo)
      else if FOptions.SearchAll and FOptions.SearchClassName and SameText(cbxObjects.Text, SAllString) and StrContains(edtMethods.Text, ProcInfo.ProcClass, False) then
        AddListItem(ProcInfo);
    end;
    if lvProcs.Items.Count = 0 then
      UpdateCodeView(nil);
  finally
    if RunningRS2009OrGreater then
      lvProcs.AlphaSort; // This no longer happens automatically?
    lvProcs.Items.EndUpdate;
    FocusAndSelectFirstItem;
  end;
  ResizeCols;
end;

procedure TfmProcedureList.FormResize(Sender: TObject);
var
  w: Integer;
begin
  w := StatusBar.Width;
  if w > 80 then begin
    StatusBar.Panels[1].Width := 80;
    StatusBar.Panels[0].Width := w - 80;
  end;
end;

// This is just a nasty hack to be sure the scroll bar is set right
// before playing with the column widths. We should fix this somehow.
procedure TfmProcedureList.ResizeCols;
begin
  PostMessage(Self.Handle, UM_RESIZECOLS, 0, 0);
end;

procedure TfmProcedureList.UMResizeCols(var Msg: TMessage);
begin
  Application.ProcessMessages;
  // this might be the "correct" way to resize the list, but it flickers a lot:
  //  TListView_Resize(lvProcs);
  // One reason for this is that this method is called very often, e.g. about 5 times
  // already when the dialog is opened.
  // So until we find a way to prevent this, we keep the following code:
  lvProcs.Columns[1].Width := Max(80, lvProcs.ClientWidth - lvProcs.Columns[2].Width
    - lvProcs.Columns[3].Width - lvProcs.Columns[0].Width);
end;

procedure TfmProcedureList.SaveSettings;
begin
  // Things that might have changed on the procedure list dialog box
  FOptions.BoundsRect := BoundsRect;
  FOptions.CodeViewWidth := pnlFunctionBody.Width;
  FOptions.CodeViewHeight := pnlFunctionBody.Height;
  FOptions.DialogFont.Assign(lvProcs.Font);
  FOptions.SaveSettings(TProcedureListExpert.GetSettings);
end;

procedure TfmProcedureList.LoadSettings;
begin
  FOptions.LoadSettings(TProcedureListExpert.GetSettings);
  BoundsRect := FOptions.BoundsRect;
  ApplyOptions(True);
  EnsureFormVisible(Self);
  ResizeCols;
end;

procedure TfmProcedureList.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveSettings;
end;

procedure TfmProcedureList.lvProcsColumnClick(Sender: TObject; Column: TListColumn);
var
  i: Integer;
  Cursor: IInterface;
begin
  i := Column.Index;
  if i <> 0 then
  begin
    Cursor := TempHourGlassCursor;
    FOptions.SortOnColumn := i;
    FillListBox;
  end;
end;

procedure TfmProcedureList.edtMethodsChange(Sender: TObject);
begin
  tmrFilter.Enabled := False;
  tmrFilter.Enabled := True; //FI:W508 - stop and restart timer on key press
end;

procedure TfmProcedureList.tmrFilterTimer(Sender: TObject);
begin
  FillListBox;
  tmrFilter.Enabled := False;
end;

procedure TfmProcedureList.edtMethodsKeyPress(Sender: TObject; var Key: Char);
begin
  case Key of
    #13:
      begin
        GotoCurrentlySelectedProcedure;
        Key := #0;
      end;
    #27:
      begin
        Close;
        Key := #0;
      end;
  end;
end;

procedure TfmProcedureList.edtMethodsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if not (((Key = VK_F4) and (ssAlt in Shift)) or
    (Key in [VK_DELETE, VK_LEFT, VK_RIGHT]) or
    ((Key in [VK_INSERT]) and ((ssShift in Shift) or (ssCtrl in Shift))) or
    ((Key in [VK_HOME, VK_END]) and (ssShift in Shift))) then
  begin
    SendMessage(lvProcs.Handle, WM_KEYDOWN, Key, 0);
    Key := 0;
  end;
end;

procedure TfmProcedureList.cbxObjectsChange(Sender: TObject);
begin
  FillListBox;
  ResizeCols;
end;

procedure TfmProcedureList.pnlHeaderResize(Sender: TObject);
begin
  pnlHeaderLeft.Width := (pnlHeader.ClientWidth div 2);
  edtMethods.Width := pnlHeaderLeft.ClientWidth - edtMethods.Left - 8;
  cbxObjects.Width := pnlHeaderRight.ClientWidth - cbxObjects.Left - 8;
end;

procedure TfmProcedureList.ClearObjectStrings;
begin
  FObjectStrings.Clear;
  FObjectStrings.Add(SAllString);
end;

procedure TfmProcedureList.LoadObjectCombobox;
var
  i: Integer;
begin
  for i := 0 to FFileScanner.Procedures.Count - 1 do
  begin
    if FFileScanner.Procedures.Items[i].ProcClass = '' then
      FObjectStrings.Add(SNoneString)
    else
      FObjectStrings.Add(FFileScanner.Procedures.Items[i].ProcClass);
  end;
  cbxObjects.Items.Assign(FObjectStrings);
  cbxObjects.ItemIndex := cbxObjects.Items.IndexOf(SAllString);
end;

{ TProcedureListExpert }

constructor TProcedureListExpert.Create;
begin
  inherited;
end;

function TProcedureListExpert.GetActionCaption: string;
resourcestring
  SMenuCaption = '&Procedure List...';
begin
  Result := SMenuCaption;
end;

function TProcedureListExpert.GetDefaultShortCut: TShortCut;
begin
  Result := Menus.ShortCut(Word('G'), [ssCtrl]);
end;

procedure TProcedureListExpert.Execute(Sender: TObject);
var
  FileName: string;
  TempFileName: string;
  Dlg: TfmProcedureList;
  Cursor: IInterface;
resourcestring
  SPasOrDprOrCPPOnly = 'This expert is for use in .pas, .dpr, .inc, .cpp, .c, and .h files only';
begin
  Cursor := TempHourGlassCursor;
  FileName := GxOtaGetCurrentSourceFile;
  if IsForm(FileName) then
  begin
    TempFileName := ChangeFileExt(FileName, '.pas');
    if GxOtaIsFileOpen(TempFileName) then
      FileName := TempFileName
    else
    begin
      TempFileName := ChangeFileExt(FileName, '.cpp');
      if GxOtaIsFileOpen(TempFileName) then
        FileName := TempFileName;
    end;
  end;

  if not (IsDprOrPas(FileName) or IsTypeLibrary(FileName) or IsInc(FileName) or
    IsCpp(FileName) or IsC(FileName) or IsH(FileName)) then begin
    MessageDlg(SPasOrDprOrCPPOnly, mtError, [mbOK], 0);
    Exit; //==>
  end;

  {$IFOPT D+} SendDebug('Procedure List: Expert activated'); {$ENDIF}
  Dlg := TfmProcedureList.CreateWithFileName(nil, FileName);
  try
    SetFormIcon(Dlg);
    if Dlg.ShowModal <> mrCancel then
      GxOtaMakeSourceVisible(FileName);
  finally
    FreeAndNil(Dlg);
  end;

  IncCallCount;
end;

function TProcedureListExpert.HasConfigOptions: Boolean;
begin
  Result := True;
end;

function TProcedureListExpert.HasMenuItem: Boolean;
begin
  Result := True;
end;

procedure TfmProcedureList.actEditCopyExecute(Sender: TObject);
var
  i: Integer;
  Procs: TStringList;
  ProcInfo: TProcedure;
begin
  if FCodeText.Focused then
  begin
    if Trim(FCodeText.SelText) <> '' then
      Clipboard.AsText := FCodeText.SelText
    else
      Clipboard.AsText := FCodeText.AsString;
  end
  else
  begin
    Procs := TStringList.Create;
    try
      for i := 0 to lvProcs.Items.Count - 1 do
      begin
        ProcInfo := TProcedure(lvProcs.Items[i].Data);
        if ProcInfo <> nil then
          Procs.Add(ProcInfo.ProcName);
      end;
    finally
      if Procs.Count > 0 then
        Clipboard.AsText := Procs.Text;
      FreeAndNil(Procs);
    end;
  end;
end;

procedure TfmProcedureList.actHelpHelpExecute(Sender: TObject);
begin
  GxContextHelp(Self, 4);
end;

procedure TfmProcedureList.actOptionsFontExecute(Sender: TObject);
begin
  dlgProcFont.Font.Assign(lvProcs.Font);
  if dlgProcFont.Execute then
    lvProcs.Font.Assign(dlgProcFont.Font);
end;

procedure TfmProcedureList.actViewStartExecute(Sender: TObject);
begin
  FOptions.SearchAll := False;
  FillListBox;
end;

procedure TfmProcedureList.actViewAnyExecute(Sender: TObject);
begin
  FOptions.SearchAll := True;
  FillListBox;
end;

procedure TfmProcedureList.actMatchClassExecute(Sender: TObject);
begin
  FOptions.SearchClassName := True;
  FillListBox;
end;

procedure TfmProcedureList.actMatchMethodExecute(Sender: TObject);
begin
  FOptions.SearchClassName := False;
  FillListBox;
end;

procedure TfmProcedureList.actViewGotoExecute(Sender: TObject);
begin
  GotoCurrentlySelectedProcedure;
end;

procedure TfmProcedureList.GotoCurrentlySelectedProcedure;
var
  ProcInfo: TProcedure;
begin
  if lvProcs.Selected <> nil then
  begin
    ProcInfo := lvProcs.Selected.Data;
    if ProcInfo <> nil then
    begin
      Assert(FEditReader <> nil);
      if FOptions.CodeViewVisible and (FCodeText.LineCount > 1) then
        FEditReader.GotoLine(ProcInfo.LineNo + FCodeText.TopLine - 1)
      else
        FEditReader.GotoLine(ProcInfo.LineNo);
      FEditReader.ShowSource;
      FEditReader.UnfoldCode;
      FEditReader.FreeFileData;
      ModalResult := mrOk;
    end;
  end;
end;

constructor TfmProcedureList.Create(AOwner: TComponent);
begin
  inherited;
  TControl_SetMinConstraints(Self);

  FMinListWidth := 500;
  FMinListHeight := 200;

  SetToolbarGradient(ToolBar);
  lvProcs.DoubleBuffered := True;
  InitializeForm;
end;

destructor TfmProcedureList.Destroy;
begin
  FreeAndNil(FCodeText);
  FreeAndNil(FObjectStrings);
  FreeAndNil(FOptions);
  FreeAndNil(FEditReader);
  inherited;
end;

procedure TfmProcedureList.InitializeForm;
begin
  SetupSyntaxHighlightingControl;

  FObjectStrings := TStringList.Create;
  FObjectStrings.Sorted := True;
  FObjectStrings.Duplicates := dupIgnore;
  ClearObjectStrings;

  FOptions := TProcedureListOptions.Create;
  FOptions.SortOnColumn := 1;

  FEditReader := TEditReader.Create(FFileName);
  FEditReader.FreeFileData;

  CenterForm(Self);

  LoadSettings;
  LoadProcs;
  FillListBox;
end;

procedure TfmProcedureList.ActionsUpdate(Action: TBasicAction; var Handled: Boolean);
begin
  actViewGoto.Enabled := (lvProcs.Selected <> nil);
  actViewStart.Checked := not FOptions.SearchAll;
  actViewAny.Checked := FOptions.SearchAll;
  actMatchClass.Checked := FOptions.SearchClassName;
  actMatchMethod.Checked := not FOptions.SearchClassName;
end;

procedure TfmProcedureList.tbnShowFunctionCodeClick(Sender: TObject);
begin
  FOptions.CodeViewVisible := not (pnlFunctionBody.Visible);
  ApplyOptions(False);
  UpdateCodeView(CurrentProcInfo);
end;

procedure TfmProcedureList.SetupSyntaxHighlightingControl;
begin
  FCodeText := TGXEnhancedEditor.Create(Self);
  if FFileScanner.Language = ltPas then
    FCodeText.HighLighter := gxpPas
  else if FFileScanner.Language = ltCpp then
    FCodeText.HighLighter := gxpCpp
  else
    FCodeText.Highlighter := gxpNone;
  FCodeText.Align := alClient;
  FCodeText.Font.Name := 'Courier New';
  FCodeText.Font.Size := 10;
  FCodeText.Parent := pnlFunctionBody;
  FCodeText.ReadOnly := True;
end;

procedure TfmProcedureList.splSeparatorCanResize(Sender: TObject; var NewSize: Integer;
  var Accept: Boolean);
begin
  if splSeparator.Align in [alLeft, alRight] then begin
    if ClientWidth - NewSize < FMinListWidth then
      NewSize := ClientWidth - FMinListWidth;
  end else begin
    // alTop, alBottom
    if ClientHeight - NewSize < FMinListHeight then
      NewSize := ClientHeight - FMinListHeight;
  end;
end;

class function TProcedureListExpert.GetName: string;
begin
  Result := 'ProcedureList'; // Do not localize.
end;

procedure TProcedureListExpert.Configure;
var
  lclOptions: TProcedureListOptions;
begin
  lclOptions := TProcedureListOptions.Create;
  lclOptions.LoadSettings(GetSettings);
  with TfmProcedureListOptions.Create(nil) do
  try
    Options := lclOptions;
    if ShowModal = mrOK then
      lclOptions.SaveSettings(GetSettings);
  finally
    Free;
    FreeAndNil(lclOptions);
  end;
end;

procedure TProcedureListExpert.UpdateAction(Action: TCustomAction);
const
  SAllowableFileExtensions =
    '.pas;.dpr;.inc;.dfm;.xfm;.nfm;.tlb;.ocx;.olb;.dll;.exe;.cpp;.c;.h'; // Do not localize.
begin
  Action.Enabled := FileMatchesExtensions(GxOtaGetCurrentSourceFile, SAllowableFileExtensions);
end;

procedure TfmProcedureList.actOptionsExecute(Sender: TObject);
begin
  with TfmProcedureListOptions.Create(nil) do
  try
    // These are adjustable in window, so update settings
    FOptions.DialogFont.Assign(lvProcs.Font);
    FOptions.CodeViewVisible := pnlFunctionBody.Visible;

    // Assign and show modal dialog, then adjust options if necessary
    Options := FOptions;
    if ShowModal = mrOK then
    begin
      ApplyOptions(False);
      FillListBox;
    end;
  finally
    Free;
  end;
end;

procedure TfmProcedureList.ApplyOptions(const bLoading: Boolean);

  procedure SetCodeViewVisibility(const bVisible: Boolean);
  begin
    pnlFunctionBody.Visible := bVisible;
    splSeparator.Visible := bVisible;
    tbnShowFunctionCode.Down := bVisible;
  end;

begin
  SetCodeViewVisibility(FOptions.CodeViewVisible);
  FCodeText.Font.Assign(FOptions.CodeViewFont);
  lvProcs.Font.Assign(FOptions.DialogFont);

  if FOptions.AlignmentChanged or bLoading then
  begin
    pnlFunctionBody.Align := FOptions.CodeViewAlignment;
    splSeparator.Align := FOptions.CodeViewAlignment;
    if FOptions.AlignmentChanged then
    begin
      case pnlFunctionBody.Align of
        alTop, alBottom: pnlFunctionBody.Height := Round(Self.Height / 2);
        alLeft, alRight: pnlFunctionBody.Width := Round(Self.Width / 2);
      end;
      FOptions.AlignmentChanged := False;
    end
    else
      case pnlFunctionBody.Align of
        alTop, alBottom: pnlFunctionBody.Height := FOptions.CodeViewHeight;
        alLeft, alRight: pnlFunctionBody.Width := FOptions.CodeViewWidth;
      end;
  end;
end;

procedure TfmProcedureList.lvProcsCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
  
  function PadNumber(const Value: string): string;
  var
    i: Integer;
  begin
    Result := Value;
    for i := Length(Value) to 5 do //FI:W528
      Result := ' ' + Result;
  end;

var
  Item1Value, Item2Value: string;
begin
  if FOptions.SortOnColumn = 3 then
  begin
    Item1Value := PadNumber(Item1.SubItems[FOptions.SortOnColumn - 1]);
    Item2Value := PadNumber(Item2.SubItems[FOptions.SortOnColumn - 1]);
  end
  else
  begin
    Item1Value := Item1.SubItems[FOptions.SortOnColumn - 1];
    Item2Value := Item2.SubItems[FOptions.SortOnColumn - 1];
  end;

  Compare := AnsiCompareText(Item1Value, Item2Value);
end;

procedure TfmProcedureList.lvProcsResize(Sender: TObject);
begin
  ResizeCols;
end;

procedure TfmProcedureList.UpdateCodeView(ProcInfo: TProcedure);
begin
  if not Assigned(FCodeText) then
    Exit;
  FCodeText.BeginUpdate;
  try
    if Assigned(ProcInfo) and (FLastProcLineNo = ProcInfo.LineNo) then
      Exit;
    FCodeText.Clear;
    if (not Assigned(ProcInfo)) or (not FOptions.CodeViewVisible) then
      Exit;
    if ProcInfo.Body <> '' then
      FCodeText.AsString := ProcInfo.Body
    else begin
      FCodeText.AsString := Copy(FUnitText, ProcInfo.BeginIndex + 1, ProcInfo.EndIndex + ProcInfo.BeginIndex);
    end;
    FLastProcLineNo := ProcInfo.LineNo;
  finally
    FCodeText.EndUpdate;
  end;
end;

function TfmProcedureList.CurrentProcInfo: TProcedure;
begin
  Result := nil;
  if Assigned(lvProcs.Selected) then
    Result := lvProcs.Selected.Data;
end;

initialization
  RegisterGX_Expert(TProcedureListExpert);

end.

