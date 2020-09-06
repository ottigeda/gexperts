unit GX_PeInformation;

{$I GX_CondDefine.inc}

interface

uses
  Windows, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, GX_PeInfo, ComCtrls, Menus,
  GX_SharedImages,
  ActnList, ToolWin, StdCtrls, SysUtils, Messages;

const
  WM_CheckParams = WM_USER + 4711;

type
  TfmPeInformation = class(TForm)
    pcMain: TPageControl;
    tshMSDOS: TTabSheet;
    tshImport: TTabSheet;
    tshExports: TTabSheet;
    tshPEHEader: TTabSheet;
    lvMSDOS: TListView;
    lvPEHeader: TListView;
    lvImports: TListView;
    splImport: TSplitter;
    lvImportFunctions: TListView;
    lvExportFunctions: TListView;
    tshPEOptional: TTabSheet;
    lvPEOptionalHeader: TListView;
    MainMenu: TMainMenu;
    mitFile: TMenuItem;
    mitFileOpen: TMenuItem;
    mitFileSep2: TMenuItem;
    mitFileExit: TMenuItem;
    mitOptions: TMenuItem;
    mitOptionsDecimal: TMenuItem;
    mitOptionsHex: TMenuItem;
    mitHelp: TMenuItem;
    mitHelpAbout: TMenuItem;
    mitFilePrint: TMenuItem;
    mitFileSep1: TMenuItem;
    mitFilePrinterSetup: TMenuItem;
    dlgPrinterSetup: TPrinterSetupDialog;
    mitHelpHelp: TMenuItem;
    mitHelpSep1: TMenuItem;
    ToolBar: TToolBar;
    tbnOpen: TToolButton;
    tbnPrint: TToolButton;
    tbnCopy: TToolButton;
    tbnHelp: TToolButton;
    Actions: TActionList;
    actFileOpen: TAction;
    actFilePrinterSetup: TAction;
    actFilePrint: TAction;
    actFileExit: TAction;
    actOptionsDecimal: TAction;
    actOptionsHex: TAction;
    actHelpHelp: TAction;
    actHelpContents: TAction;
    actHelpAbout: TAction;
    mitHelpContents: TMenuItem;
    actEditCopy: TAction;
    mitEdit: TMenuItem;
    mitEditCopy: TMenuItem;
    tbnSep1: TToolButton;
    tbnSep2: TToolButton;
    tshVersionInfo: TTabSheet;
    lvVersionInfo: TListView;
    tshPackageInfo: TTabSheet;
    splPackageInfo: TSplitter;
    lbPackageInfoType: TListBox;
    lbPackageInfo: TListBox;
    procedure lvImportsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure FormResize(Sender: TObject);
    procedure pcMainChange(Sender: TObject);
    procedure lvMSDOSData(Sender: TObject; Item: TListItem);
    procedure lvPEHeaderData(Sender: TObject; Item: TListItem);
    procedure lvPEOptionalHeaderData(Sender: TObject; Item: TListItem);
    procedure lvImportsData(Sender: TObject; Item: TListItem);
    procedure lvExportFunctionsData(Sender: TObject; Item: TListItem);
    procedure lvImportFunctionsData(Sender: TObject; Item: TListItem);
    procedure actEditCopyExecute(Sender: TObject);
    procedure actFileOpenExecute(Sender: TObject);
    procedure actFilePrinterSetupExecute(Sender: TObject);
    procedure actFilePrintExecute(Sender: TObject);
    procedure actFileExitExecute(Sender: TObject);
    procedure actHelpHelpExecute(Sender: TObject);
    procedure actHelpAboutExecute(Sender: TObject);
    procedure actOptionsDecimalExecute(Sender: TObject);
    procedure actOptionsHexExecute(Sender: TObject);
    procedure actHelpContentsExecute(Sender: TObject);
    procedure ActionsUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure lbPackageInfoTypeClick(Sender: TObject);
    procedure lvExportFunctionsColumnClick(Sender: TObject; Column: TListColumn);
    procedure lvExportFunctionsKeyPress(Sender: TObject; var Key: Char);
  private
    PEInfo: TPEFileInfo;
    FNumberType: TNumberType;
    FFileName: string;
    FBlockEvents: Boolean;
    FExportsColumnToSortOn: Integer;
    FExportsColumnSortedList: TList;
    FExportsFilter: string;
    procedure LoadPEInfo(const AFileName: string);
    procedure SaveSettings;
    procedure LoadSettings;
    procedure HandleFilesDropped(_Sender: TObject; _Files: TStrings);
    procedure SetNumberType(const Value: TNumberType);
    procedure SetVersionInfo(const AFilename: string);
    procedure SetPackageInfo(const AFilename: string);
    procedure WmCheckParams(var _Msg: TMessage); message WM_CheckParams;
    function EventsAllowedAndAllAssigned(_Item: TListItem): Boolean;
    function CompareExportRows(_RowIdx1, _RowIdx2: Integer): Integer;
    procedure UpdateExportsFilter;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property FileName: string read FFileName;
    property NumberType: TNumberType read FNumberType write SetNumberType;
  end;

var
  fmPeInformation: TfmPeInformation;

implementation

{$R *.dfm}

uses
  StrUtils,
  Math,
  Clipbrd,
  u_dzVersionInfo,
  u_dzStringUtils,
  u_dzTypes,
  u_dzPackageInfo,
  u_dzClassUtils,
  u_dzVclUtils,
  GX_GenericUtils,
  GX_DbugIntf,
  GX_PeInfoPrint;

procedure SetListViewItem(AItem: TListItem; AValue: string);
var
  Arr: TStringArray;
  i: Integer;
begin
  Arr := SplitString(AValue, #9);
  if Length(Arr) > 0 then begin
    AItem.Caption := Arr[0];
    for i := 1 to High(Arr) do
      AItem.SubItems.Add(Arr[i]);
  end;
end;

procedure SetListViewItems(ListView: TListView; Items: TStrings);
begin
  Assert(Assigned(ListView));
  Assert(Assigned(Items));

  ListView.Items.BeginUpdate;
  try
    ListView.Items.Clear;
    ListView.Items.Count := Items.Count;
  finally
    ListView.Items.EndUpdate;
  end;
end;

procedure TfmPeInformation.LoadPEInfo(const AFileName: string);
resourcestring
  SFormCaption = 'PE Information - ';
  SPENoDirectories = 'PE information is not available for directories';
begin
  lvVersionInfo.Items.Clear;
  TListbox_ClearWithObjects(lbPackageInfoType);
  lbPackageInfo.Items.Clear;

  if DirectoryExists(AFileName) then
    raise Exception.Create(SPENoDirectories);
  Screen.Cursor := crHourglass;
  FBlockEvents := True;
  try
    Self.FFileName := AFileName;
    Caption := SFormCaption + ExtractFileName(AFileName);
    pcMain.ActivePage := tshMSDOS;

    FreeAndNil(PEInfo);

    PEInfo := TPEFileInfo.Create(AFileName, NumberType);
    lvImportFunctions.Items.Clear;
    SetListViewItems(lvMSDOS, PEInfo.MSDOSHeader);
    SetListViewItems(lvPEHeader, PEInfo.PEHeaderList);
    SetListViewItems(lvPEOptionalHeader, PEInfo.PEOptionalHeaderList);
    SetListViewItems(lvImports, PEInfo.ImportList);
    SetListViewItems(lvExportFunctions, PEInfo.ExportList);

    SetVersionInfo(AFilename);
    SetPackageInfo(AFilename);

  finally
    FBlockEvents := False;
    Screen.Cursor := crDefault;
  end;
  FormResize(Self);
end;

procedure TfmPeInformation.SetVersionInfo(const AFilename: string);
var
  VerItems: TListItems;
  st: TStringList;
  i: Integer;

  procedure AddItem(const ACaption, AValue: string);
  var
    li: TListItem;
  begin
      li := VerItems.Add;
      li.Caption := ACaption;
      li.SubItems.Add(AValue);
  end;

resourcestring
  SNoVersionInfo = 'no version info';
var
  VerInfo: IFileInfo;
begin
  VerInfo := TFileInfo.Create(AFileName);
  VerInfo.AllowExceptions := False;

  VerItems := lvVersionInfo.Items;
  VerItems.BeginUpdate;
  try
    VerItems.Clear;
    if not VerInfo.HasVersionInfo then begin
      AddItem(SNoVersionInfo, '');
    end else begin
      AddItem('Filename', VerInfo.Filename);
      AddItem('FileDir', VerInfo.FileDir);
      AddItem('Description', VerInfo.FileDescription);
      AddItem('Version', VerInfo.FileVersion);
      AddItem('Product', VerInfo.ProductName);
      AddItem('Product Version', VerInfo.ProductVersion);
      AddItem('Company', VerInfo.CompanyName);
      AddItem('Copyright', VerInfo.LegalCopyRight);
      AddItem('Trademarks', VerInfo.LegalTradeMarks);
      AddItem('Internal Name', VerInfo.InternalName);
      AddItem('Original Filename', VerInfo.OriginalFilename);
      AddItem('Comments', VerInfo.Comments);
      AddItem('PrivateBuild', VerInfo.PrivateBuild);
      AddItem('SpecialBuild', VerInfo.SpecialBuild);
      st := TStringList.Create;
      try
        VerInfo.GetAllStrings(st);
        for i := 0 to st.Count - 1 do
          AddItem(st.Names[i], st.Values[st.Names[i]]);
      finally
        FreeAndNil(st);
      end;
    end;
  finally
    VerItems.EndUpdate;
  end;
end;

procedure TfmPeInformation.WmCheckParams(var _Msg: TMessage);
begin
  if ParamCount >0 then
    LoadPEInfo(ParamStr(1));
end;

procedure TfmPeInformation.SetPackageInfo(const AFilename: string);
var
  sl: TStringList;
  pitItems: TStrings;
  Info: TPackageInfo;
  i: Integer;
  s: string;
  p: Integer;
begin
  pitItems := lbPackageInfoType.Items;
  pitItems.BeginUpdate;
  try
    TListbox_ClearWithObjects(lbPackageInfoType);
    try
      Info := TPackageInfo.Create(AFileName);
      try
        sl := TStringList.Create;
        sl.Add(Info.Description);
        pitItems.AddObject('Description', sl);

        sl := TStringList.Create;
        sl.Assign(Info.Units);
        pitItems.AddObject('Units', sl);

        sl := TStringList.Create;
        sl.Assign(Info.Required);
        pitItems.AddObject('Required Packages', sl);

        sl := TStringList.Create;
        for i := 0 to PEInfo.ExportList.Count - 1 do begin
          s := PEInfo.ExportList[i];
          if StartsText('@$xp$', s) then begin
            s := Copy(s, 2, 255);
            p := Pos('@', s);
            if p > 0 then begin
              s := Copy(s, p + 1, 255);
              p := Pos(#$09, s);
              if p = 0 then
                p := Pos('$', s);
              if p > 0 then
                s := Copy(s, 1, p - 1);
              sl.Add(s);
            end;
          end;
        end;
        pitItems.AddObject('Exported Classes', sl);
      finally
        FreeAndNil(Info);
      end;
    except
      pitItems.Add('not available');
    end;
  finally
    pitItems.EndUpdate;
  end;
  lbPackageInfoType.ItemIndex := 0;
  lbPackageInfoType.OnClick(lbPackageInfoType);
end;

procedure TfmPeInformation.lvImportsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
var
  ImpExp: TImportExport;
begin
  if Change = ctState then
  begin
    lvImportFunctions.Items.BeginUpdate;
    try
      try
        lvImportFunctions.Items.Clear;

        if lvImports.Selected = nil then
          Exit;

        ImpExp := TImportExport(PEInfo.ImportList.Objects[lvImports.Selected.Index]);
        Assert(Assigned(ImpExp));
        lvImportFunctions.Items.Count := ImpExp.Count;
      except
        on E: Exception do
          GxLogAndShowException(E);
      end;
    finally
      lvImportFunctions.Items.EndUpdate;
    end;
  end;
  FormResize(Self);
end;

procedure TfmPeInformation.FormResize(Sender: TObject);
begin
  try
    with lvMSDOS do
      Columns.Items[1].Width := Max(ClientWidth - Columns.Items[0].Width - 1, 0);
    with lvPEHeader do
      Columns.Items[1].Width := Max(ClientWidth - Columns.Items[0].Width - 1, 0);
    with lvPEOptionalHeader do
      Columns.Items[1].Width := Max(ClientWidth - Columns.Items[0].Width - 1, 0);
    with lvImportFunctions do
      Columns.Items[0].Width := Max(ClientWidth - Columns.Items[1].Width - 1, 0);
    with lvExportFunctions do
      Columns.Items[0].Width := Max(ClientWidth - Columns.Items[1].Width - Columns.Items[2].Width - 1, 0);
    with lvVersionInfo do
      Columns.Items[1].Width := Max(ClientWidth - Columns.Items[0].Width - 1, 0);
  except
    on E: Exception do
    begin
      // Swallow exceptions.
    end;
  end;
end;

procedure TfmPeInformation.lbPackageInfoTypeClick(Sender: TObject);
var
  sl: TStringList;
  Idx: Integer;
  InfoItems: TStrings;
begin
  InfoItems := lbPackageInfo.Items;
  InfoItems.BeginUpdate;
  try
    InfoItems.Clear;
    Idx := lbPackageInfoType.ItemIndex;
    if Idx = -1 then
      Exit;
    sl := lbPackageInfoType.Items.Objects[Idx] as TStringList;
    if not Assigned(sl) then
      Exit;
    InfoItems.Assign(sl);
  finally
    InfoItems.EndUpdate;
  end;
end;

procedure TfmPeInformation.SetNumberType(const Value: TNumberType);
begin
  FNumberType := Value;

  if PEInfo <> nil then
    LoadPEInfo(FFileName);
end;

procedure TfmPeInformation.SaveSettings;
var
  RegEntry: TRegistryEntry;
  s: string;
begin
  RegEntry := TForm_GetPlacementRegistryEntry(Self, 'GExperts');
  TForm_StorePlacement(Self, fpePosAndSize, RegEntry);

  RegEntry.KeyName := TApplication_GetConfigRegistryPath('GExperts');
  TRegistry_WriteInteger(RegEntry.KeyName, 'Numbers', Integer(NumberType));

  s := ExtractFilePath(FFileName);
  if s <> '\' then
    TRegistry_WriteString(RegEntry.KeyName, 'BinPath', ExtractFilePath(FFileName));
end;

procedure TfmPeInformation.LoadSettings;
var
  RegEntry: TRegistryEntry;
begin
  RegEntry := TForm_GetPlacementRegistryEntry(Self, 'GExperts');
  TForm_ReadPlacement(Self, fpePosAndSize, RegEntry);

  RegEntry.KeyName := TApplication_GetConfigRegistryPath('GExperts');
  NumberType := TNumberType(TRegistry_ReadInteger(RegEntry.KeyName, 'Numbers', Ord(ntHex)));

  FFileName := TRegistry_ReadString(RegEntry.KeyName, 'BinPath', '');
  FFileName := AddSlash(FFileName) + 'SomeExecutable.exe';

  EnsureFormVisible(Self);
end;

procedure TfmPeInformation.pcMainChange(Sender: TObject);
begin
  // Let the listview update so the columns size right
  Application.ProcessMessages;
  FormResize(Self);
end;

procedure TfmPeInformation.HandleFilesDropped(_Sender: TObject; _Files: TStrings);
begin
  if _Files.Count >= 0 then
    LoadPEInfo(_Files[0]);
end;

function TfmPeInformation.EventsAllowedAndAllAssigned(_Item: TListItem): Boolean;
begin
  Result := not FBlockEvents
    and Assigned(_Item)
    and Assigned(PEInfo)
    and Assigned(PEInfo.MSDOSHeader);
end;

procedure TfmPeInformation.lvMSDOSData(Sender: TObject; Item: TListItem);
begin
  if EventsAllowedAndAllAssigned(Item) then
    SetListViewItem(Item, PEInfo.MSDOSHeader[Item.Index]);
end;

procedure TfmPeInformation.lvPEHeaderData(Sender: TObject; Item: TListItem);
begin
  if EventsAllowedAndAllAssigned(Item) then
    SetListViewItem(Item, PEInfo.PEHeaderList[Item.Index]);
end;

procedure TfmPeInformation.lvPEOptionalHeaderData(Sender: TObject; Item: TListItem);
begin
  if EventsAllowedAndAllAssigned(Item) then
    SetListViewItem(Item, PEInfo.PEOptionalHeaderList[Item.Index]);
end;

procedure TfmPeInformation.lvImportsData(Sender: TObject; Item: TListItem);
begin
  if EventsAllowedAndAllAssigned(Item) then
    SetListViewItem(Item, PEInfo.ImportList[Item.Index]);
end;

function TfmPeInformation.CompareExportRows(_RowIdx1, _RowIdx2: Integer): Integer;
var
  Arr1: TStringArray;
  Arr2: TStringArray;
  ColIdx: Integer;
  s1: string;
  s2: string;
  ExpList: TStrings;
begin
  ExpList := PEInfo.ExportList;
  Arr1 := SplitString(ExpList[_RowIdx1], #9);
  Arr2 := SplitString(ExpList[_RowIdx2], #9);

  ColIdx := Abs(FExportsColumnToSortOn) - 1;
  if ColIdx < Length(Arr1) then
    s1 := Arr1[ColIdx]
  else
    s1 := '';

  if ColIdx < Length(Arr2) then
    s2 := Arr2[ColIdx]
  else
    s2 := '';
  case ColIdx of
    1, 2: begin
        if s1 = '' then
          s1 := '0';
        if s2 = '' then
          s2 := '0';
        if FExportsColumnToSortOn < 0 then
          Result := CompareValue(StrToint(s1), StrToint(s2))
        else
          Result := CompareValue(StrToint(s1), StrToint(s2));
      end;
  else // 0: begin
    if FExportsColumnToSortOn < 0 then
      Result := CompareText(s2, s1)
    else
      Result := CompareText(s1, s2);
  end;
end;

function ExportsCompareFunc(Item1, Item2: Pointer): Integer;
var
  Idx1: Integer absolute Item1;
  Idx2: Integer absolute Item2;
begin
  Result := fmPeInformation.CompareExportRows(Idx1 - 1, Idx2 - 1);
end;

procedure TfmPeInformation.lvExportFunctionsColumnClick(Sender: TObject; Column: TListColumn);
begin
  if not Assigned(PEInfo) then
    Exit; //==>
  if FExportsColumnToSortOn = Column.Index + 1 then
    FExportsColumnToSortOn := -FExportsColumnToSortOn
  else
    FExportsColumnToSortOn := Column.Index + 1;
  UpdateExportsFilter;
end;

procedure TfmPeInformation.UpdateExportsFilter;
var
  i: Integer;
  ExpList: TStrings;
  s: string;
begin
  if FExportsFilter = '' then
    s := '<enter text to filter>'
  else
    s := '[' + FExportsFilter + ']';
  lvExportFunctions.Columns[0].Caption := 'Name ' + s;

  if not Assigned(FExportsColumnSortedList) then
    FExportsColumnSortedList := TList.Create
  else
    FExportsColumnSortedList.Clear;

  ExpList := PEInfo.ExportList;
  for i := 0 to PEInfo.ExportList.Count - 1 do begin
    if (FExportsFilter = '') or StrContains(FExportsFilter, ExpList[i], False) then
      FExportsColumnSortedList.Add(Pointer(i + 1));
  end;

  FExportsColumnSortedList.Sort(ExportsCompareFunc);

  lvExportFunctions.Items.Count := FExportsColumnSortedList.Count;
  lvExportFunctions.Invalidate;
end;

procedure TfmPeInformation.lvExportFunctionsData(Sender: TObject; Item: TListItem);
var
  Idx: Integer;
begin
  if not EventsAllowedAndAllAssigned(Item) then
    Exit; //==>
  Idx := Item.Index;
  if Assigned(FExportsColumnSortedList) then
    Idx := Integer(FExportsColumnSortedList[Idx]) - 1;
  SetListViewItem(Item, PEInfo.ExportList[Idx]);
end;

procedure TfmPeInformation.lvExportFunctionsKeyPress(Sender: TObject; var Key: Char);
const
  EscChar = Char(VK_ESCAPE);
  BackspaceChar = Char(vk_back);
begin
  if Key = EscChar then
    FExportsFilter := ''
  else   if Key = BackspaceChar then begin
    if FExportsFilter <> '' then
      FExportsFilter := LeftStr(FExportsFilter, Length(FExportsFilter) - 1);
  end else
    FExportsFilter := FExportsFilter + Key;
  UpdateExportsFilter;
end;

procedure TfmPeInformation.lvImportFunctionsData(Sender: TObject; Item: TListItem);
var
  ImpExp: TImportExport;
  SelectedListItem: TListItem;
  Idx: Integer;
begin
  if not EventsAllowedAndAllAssigned(Item) then
    Exit; //==>
  SelectedListItem := lvImports.Selected;
  if not Assigned(SelectedListItem) then
    Exit; //==>

  ImpExp := TImportExport(PEInfo.ImportList.Objects[SelectedListItem.Index]);

  Assert(Assigned(ImpExp));

  Idx := Item.Index;
  Item.Caption := ImpExp.Items[Idx].FunctionName;
  Item.SubItems.Add(PEInfo.IntToNum(ImpExp.Items[Idx].Ordinal));
end;

procedure TfmPeInformation.actEditCopyExecute(Sender: TObject);
var
  List: TListView;
  i, j: Integer;
  ItemString: string;
  PELines: TStringList;
begin
  List := nil;
  if ActiveControl is TListView then
    List := ActiveControl as TListView
  else begin
    if pcMain.ActivePage = tshPackageInfo then begin
      PELines := TStringList.Create;
      try
        for i := 0 to lbPackageInfoType.Items.Count - 1 do begin
          if i > 0 then
            PELines.Add('');
          PELines.Add(lbPackageInfoType.Items[i] + ':');
          PELines.AddStrings(TStrings(lbPackageInfoType.Items.Objects[i]));
        end;
        Clipboard.AsText := PELines.Text;
      finally
        PELines.Free;
      end;
      Exit;
    end;

    for i := pcMain.ActivePage.ControlCount - 1 downto 0 do
      if pcMain.ActivePage.Controls[i] is TListView then
      begin
        List := pcMain.ActivePage.Controls[i] as TListView;
        Break;
      end;
  end;
  if (List = nil) then
    Exit;

  PELines := TStringList.Create;
  try
    for i := 0 to List.Items.Count - 1 do
    begin
      ItemString := List.Items.Item[i].Caption;
      for j := 0 to List.Items.Item[i].SubItems.Count - 1 do
        ItemString := ItemString + #09 + List.Items.Item[i].SubItems.Strings[j];
      PELines.Add(ItemString);
    end;
    Clipboard.AsText := PELines.Text;
  finally
    FreeAndNil(PELines);
  end;
end;

procedure TfmPeInformation.actFileOpenExecute(Sender: TObject);
var
  fn: string;
begin
  fn := FFileName;
  if ShowOpenDialog('Open file to examine', 'exe', fn,
    'PE Binary Files (*.exe, *.dll, *.bpl, *.dpl, *.ocx)|*.exe;*.dll;*.bpl;*.dpl;*.ocx|'
    + 'EXE Files (*.exe)|*.EXE|'
    + 'DLL Files (*.dll)|*.dll|'
    + 'CodeGear Packages (*.bpl, *.dpl)|*.dpl;*.bpl|'
    + 'OCX Controls (*.ocx)|*.ocx') then
    LoadPEInfo(fn);
end;

procedure TfmPeInformation.actFilePrinterSetupExecute(Sender: TObject);
begin
  dlgPrinterSetup.Execute;
end;

procedure TfmPeInformation.actFilePrintExecute(Sender: TObject);
resourcestring
  SPeInfoFor = 'PE Information for ';
  SMsDosHeader = 'MS-DOS Header';
  SPeHeader = 'PE Header';
  SPeOptionalHeader = 'PE Optional Header';
  SImports = 'Imports';
  SFunction = 'Function';
  SOrdinal = 'Ordinal';
  SExports = 'Exports';
  SVersionInfo = 'Version Info';
  SPackageInfo = 'Package Info';
  SNoPackageInformationAvailable = 'No package information available';

var
  RichEdit: TRichEdit;

  procedure PrintHeader(LV: TListView; const Header: string);
  var
    i: Integer;
    Line: string;
  begin
    with RichEdit do
    begin
      RichEdit.SelAttributes.Style := [fsBold];
      Lines.Add(Header);

      RichEdit.SelAttributes.Style := [];
      for i := 0 to LV.Items.Count - 1 do
      begin
        Line := LV.Items[i].Caption;
        if LV.Items[i].SubItems.Count > 0 then
          Line := Line + #9 + ':   ' + LV.Items[i].SubItems[0];
        Lines.Add('   ' + Line);
      end;
      Lines.Add('');
    end;
  end;

var
  Tabs: TPeInfoTabSet;
  Line: string;
  i, j: Integer;
  ImpExp: TImportExport;
  sl: TStrings;
  li: TListItem;
begin
  if PEInfo = nil then
    Exit;

  if pcMain.ActivePage = tshMSDOS then
    Tabs := [pitMsDos]
  else if pcMain.ActivePage = tshPEHEader then
    Tabs := [pitPeHeader]
  else if pcMain.ActivePage = tshPEHEader then
    Tabs := [pitPeHeader]
  else if pcMain.ActivePage = tshPEOptional then
    Tabs := [pitPeOptHeader]
  else if pcMain.ActivePage = tshImport then
    Tabs := [pitImports]
  else if pcMain.ActivePage = tshExports then
    Tabs := [pitExports]
  else if pcMain.ActivePage = tshVersionInfo then
    Tabs := [pitVersionInfo]
  else
    Tabs := [pitPackageInfo];

  if not Tf_PeInfoPrint.Execute(Self, Tabs) or (Tabs = []) then
    Exit;

  try

    RichEdit := TRichEdit.Create(Self);
    Screen.Cursor := crHourglass;
    try
      RichEdit.Visible := False;
      RichEdit.Parent := Self;
      RichEdit.Clear;
      RichEdit.DefAttributes.Name := 'Arial';
      RichEdit.DefAttributes.Size := 10;

      RichEdit.Paragraph.TabCount := 1;
      RichEdit.Paragraph.Tab[0]   := 200;

      // Document header
      RichEdit.Lines.Add('PE Header information for ' + FFileName);
      // AJB: I would like some file info here, date/time, version...
      RichEdit.Lines.Add('');

      if pitMsDos in Tabs then begin
        // MS-DOS Header
        PrintHeader(lvMSDOS, SMsDosHeader);
      end;

      if pitPeHeader in Tabs then begin
        // PE Header
        PrintHeader(lvPEHeader, SPeHeader);
      end;

      if pitPeOptHeader in Tabs then begin
        // PE Optional Header
        PrintHeader(lvPEOptionalHeader, SPeOptionalHeader);
      end;

      if pitImports in Tabs then begin
        // Imports
        RichEdit.Paragraph.TabCount := 2;
        RichEdit.Paragraph.Tab[0]   := 80;
        RichEdit.Paragraph.Tab[1]   := 300;
        RichEdit.SelAttributes.Style := [fsBold];
        RichEdit.SelText := SImports;

        for j := 0 to lvImports.Items.Count - 1 do
        begin
          RichEdit.SelAttributes.Style := [fsUnderline];
          Line := PEInfo.ImportList[j] + #09 + SFunction + #09 + SOrdinal;
          RichEdit.Lines.Add('   ' + Line + '   ');
          RichEdit.SelAttributes.Style := [];

          ImpExp := TImportExport(PEInfo.ImportList.Objects[j]);

          for i := 0 to ImpExp.Count - 1 do
          begin
            Line := ImpExp.Items[i].FunctionName;
            if Length(Line) > 32 then
              Line := Copy(Line, 1, 32) + '...';
            RichEdit.Lines.Add(#09 + Line + #09 + IntToStr(ImpExp.Items[i].Ordinal));
          end;
          RichEdit.Lines.Add('');
        end;
      end;

      if pitExports in Tabs then begin
        // Exports
        RichEdit.Paragraph.TabCount := 3;
        RichEdit.Paragraph.Tab[0]   := 20;
        RichEdit.Paragraph.Tab[1]   := 280;
        RichEdit.Paragraph.Tab[2]   := 380;

        RichEdit.SelAttributes.Style := [fsBold];
        RichEdit.SelText := SExports;
        RichEdit.SelAttributes.Style := [];
        for i := 0 to lvExportFunctions.Items.Count - 1 do
        begin
          li := lvExportFunctions.Items[i];
          Line := li.Caption + #09 + li.SubItems[0] + #09 + li.SubItems[1];
          RichEdit.Lines.Add(#09 + Line);
        end;

        RichEdit.Lines.Add('');
        RichEdit.Lines.Add('');
      end;

      if pitVersionInfo in Tabs then begin
        // Version information
        RichEdit.Paragraph.TabCount := 1;
        RichEdit.Paragraph.Tab[0]   := 200;

        PrintHeader(lvVersionInfo, SVersionInfo);

        RichEdit.Lines.Add('');
      end;

      if pitPackageInfo in Tabs then begin
        // Package Info
        RichEdit.Paragraph.TabCount := 1;
        RichEdit.Paragraph.Tab[0]   := 100;
        RichEdit.SelAttributes.Style := [fsBold];
        RichEdit.SelText := SPackageInfo;

        if lbPackageInfoType.Items.Count = 0 then begin
          RichEdit.SelAttributes.Style := [];
          RichEdit.Lines.Add(SNoPackageInformationAvailable);
          RichEdit.Lines.Add('');
        end else begin
          for j := 0 to lbPackageInfoType.Items.Count - 1 do begin
            RichEdit.SelAttributes.Style := [fsUnderline];
            RichEdit.Lines.Add(lbPackageInfoType.Items[j]);

            RichEdit.SelAttributes.Style := [];
            sl := TStrings(lbPackageInfoType.Items.Objects[j]);
            for i := 0 to sl.Count - 1 do begin
              Line := sl[i];
              RichEdit.Lines.Add(#09 + Line);
            end;
            RichEdit.Lines.Add('');
          end;
        end;
      end;

      RichEdit.Print(SPeInfoFor + ExtractFileName(FFileName));
    finally
      Screen.Cursor := crDefault;
      FreeAndNil(RichEdit);
    end;
  except
    on E: Exception do
      GxLogAndShowException(E);
  end;

end;

procedure TfmPeInformation.actFileExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfmPeInformation.actHelpHelpExecute(Sender: TObject);
begin
//  GxContextHelp(Self, 16);
end;

procedure TfmPeInformation.actHelpAboutExecute(Sender: TObject);
begin
//  ShowGXAboutForm;
end;

procedure TfmPeInformation.actOptionsDecimalExecute(Sender: TObject);
begin
  NumberType := ntDecimal;
end;

procedure TfmPeInformation.actOptionsHexExecute(Sender: TObject);
begin
  NumberType := ntHex;
end;

constructor TfmPeInformation.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  HandleNeeded;

  SetToolbarGradient(ToolBar);

  TControl_SetMinConstraints(Self);

  u_dzVclUtils.TWinControl_ActivateDropFiles(Self, HandleFilesDropped);

  pcMain.ActivePage := tshMSDOS;
  CenterForm(Self);
  LoadSettings;

  FExportsColumnToSortOn := 1;

  PostMessage(Handle, WM_CheckParams, 0, 0);
end;

destructor TfmPeInformation.Destroy;
begin
  SaveSettings;

  FreeAndNil(FExportsColumnSortedList);
  FreeAndNil(PEInfo);

  TListbox_ClearWithObjects(lbPackageInfoType);

//  if Assigned(FileDrop) then
//  begin
//    FileDrop.Unregister;
//    FreeAndNil(FileDrop);
//  end;

  inherited Destroy;

  fmPeInformation := nil;
end;

procedure TfmPeInformation.actHelpContentsExecute(Sender: TObject);
begin
//  GxContextHelpContents(Self);
end;

procedure TfmPeInformation.ActionsUpdate(Action: TBasicAction; var Handled: Boolean);
begin
  actOptionsDecimal.Checked := (NumberType = ntDecimal);
  actOptionsHex.Checked := (NumberType = ntHex);
end;

end.

