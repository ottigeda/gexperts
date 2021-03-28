unit GX_eIfDef;

{$I GX_CondDefine.inc}

interface

uses
  Windows,
  SysUtils,
  Classes,
  Controls,
  Forms,
  StdCtrls,
  ComCtrls,
  Grids,
  ExtCtrls,
  Graphics,
  ActnList,
  Actions,
  Menus,
  Messages,
  Contnrs,
  u_dzCompilerAndRtlVersions,
  GX_BaseForm,
  GX_GenericUtils;

{$IF RTLVersion <= RtlVersionDelphi2005}
// Delphi < 2006 does not have the MouseLeave event so we implement it via
// an interposer class using http://stackoverflow.com/a/3182185/49925
type
  TPageControl = class(ComCtrls.TPageControl)
  private
    FMouseTracking: Boolean;
    FOnMouseLeave: TNotifyEvent;
    procedure WMMouseLeave(var Msg: TMessage); message WM_MOUSELEAVE;
  protected
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  published
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  end;
{$IFEND}

type
  TfmConfigureIfDef = class(TfmBaseForm)
    pc_IfClasses: TPageControl;
    p_Bottom: TPanel;
    b_Ok: TButton;
    b_Cancel: TButton;
    chk_AppendComment: TCheckBox;
    b_Open: TButton;
    b_Add: TButton;
    TheActionList: TActionList;
    act_Open: TAction;
    act_Add: TAction;
    pm_IncludeFiles: TPopupMenu;
    procedure pc_IfClassesChange(Sender: TObject);
    procedure chk_AppendCommentClick(Sender: TObject);
    procedure act_OpenExecute(Sender: TObject);
    procedure act_AddExecute(Sender: TObject);
    procedure pc_IfClassesMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure pc_IfClassesMouseLeave(Sender: TObject);
  private
    FText: string;
    FSearchPath: TStringList;
    FFindThread: TFileFindThread;
    FTabDefinitions: TObjectList;
    procedure InitCompilerVersion;
    procedure InitVerXxx;
    procedure InitOptions;
    procedure InitRtlVersion;
    procedure InitIncludes;
    procedure IncludeFilesListReady;
    procedure OnInludeFileSelected(_Sender: TObject);
    function AddIncludePage(_No: Integer; const _FullFn: string; _IsIncluded: Boolean): TTabSheet;
    function IsKnownIncFile(const _fn: string): Boolean;
  public
    class function Execute(_bmp: TBitmap; var _AppendComment: Boolean;
      out _Text: string; out _IncludeFile: string): Boolean;
    constructor Create(_Owner: TComponent); override;
    destructor Destroy; override;
    function GetIncludeFile: string;
  end;

implementation

{$R *.dfm}

uses
  ToolsAPI,
  StrUtils,
  u_dzVclUtils,
  u_dzStringUtils,
  GX_BaseExpert,
  GX_EditorExpert,
  GX_ConfigurationInfo,
  GX_OtaUtils;

{ TIfDefExpert }

type
  TIfDefExpert = class(TEditorExpert)
  private
    FAppendComment: Boolean;
  protected
    procedure InternalLoadSettings(_Settings: IExpertSettings); override;
    procedure InternalSaveSettings(_Settings: IExpertSettings); override;
  public
    class function GetName: string; override;
    constructor Create; override;
    function GetDisplayName: string; override;
    procedure Execute(Sender: TObject); override;
    function GetHelpString: string; override;
    function HasConfigOptions: Boolean; override;
  end;

constructor TIfDefExpert.Create;
begin
  inherited Create;
end;

procedure TIfDefExpert.Execute(Sender: TObject);
var
  InsertString: string;
  IncFile: string;
  Lines: TGXUnicodeStringList;
  i: Integer;
  s: string;
  CurPos: TOTAEditPos;
  EditPos: TOTAEditPos;
begin
  if not TfmConfigureIfDef.Execute(GetBitmap, FAppendComment, InsertString, IncFile) then
    Exit; //==>

  IncCallCount;

  GxOtaInsertLineIntoEditor(InsertString);

  if IncFile <> '' then begin
    // if an include file is necessary for the ifdef, add the include file at the beginning
    CurPos := GxOtaGetCurrentEditPos();
    Lines := TGXUnicodeStringList.Create;
    try
      if not GxOtaGetActiveEditorText(Lines, False) then
        Exit; //==>
      for i := 0 to Lines.Count - 1 do begin
        s := Lines[i];
        s := Trim(s);
        if SameText(s, 'INTERFACE') then begin
          EditPos.Col := 1;
          EditPos.Line := i + 2; // +1 for index -> number, +1 for next line
          GxOtaGotoEditPos(EditPos);
          GxOtaInsertLineIntoEditor('{$I ' + IncFile + '}' + CRLF);
          if CurPos.Line > EditPos.Line then
            Inc(CurPos.Line);
          GxOtaGotoEditPos(CurPos);
          Exit; //==>
        end;
      end;
    finally
      FreeAndNil(Lines);
    end;
  end;
end;

function TIfDefExpert.GetDisplayName: string;
resourcestring
  SIfDefExpertName = 'IF directive';
begin
  Result := SIfDefExpertName;
end;

function TIfDefExpert.GetHelpString: string;
resourcestring
  SIfDefExpertHelp =
    '  This expert inserts a {$IF/IFDEF/IFNDEF} directive into the source code';
begin
  Result := SIfDefExpertHelp;
end;

class function TIfDefExpert.GetName: string;
begin
  Result := 'IfDef';
end;

function TIfDefExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

procedure TIfDefExpert.InternalLoadSettings(_Settings: IExpertSettings);
begin
  inherited InternalLoadSettings(_Settings);

  // Do not localize any of the below items
  FAppendComment := _Settings.ReadBool('AppendComment', False);
end;

procedure TIfDefExpert.InternalSaveSettings(_Settings: IExpertSettings);
begin
  inherited InternalSaveSettings(_Settings);

  // Do not localize any of the below items
  _Settings.WriteBool('AppendComment', FAppendComment);
end;

{ TfmConfigureIfDef }

class function TfmConfigureIfDef.Execute(_bmp: TBitmap; var _AppendComment: Boolean;
  out _Text: string; out _IncludeFile: string): Boolean;
var
  frm: TfmConfigureIfDef;
begin
  frm := TfmConfigureIfDef.Create(Application);
  try
    ConvertBitmapToIcon(_bmp, frm.Icon);
    frm.chk_AppendComment.Checked := _AppendComment;
    Result := frm.ShowModal = mrOk;
    if Result then begin
      _AppendComment := frm.chk_AppendComment.Checked;
      _Text := frm.FText;
      _IncludeFile := frm.GetIncludeFile;
    end;
  finally
    FreeAndNil(frm);
  end;
end;

constructor TfmConfigureIfDef.Create(_Owner: TComponent);
var
  r: TRect;
begin
  inherited;

  p_Bottom.BevelOuter := bvNone;

  TControl_SetMinConstraints(Self);

  pc_IfClasses.OnMouseLeave := pc_IfClassesMouseLeave;

  FSearchPath := TStringList.Create;
  GxOtaGetEffectiveLibraryPath(FSearchPath);

  FTabDefinitions := TObjectList.Create;

  // This searches for any .inc file in the path that may not have been included
  // in the current unit. These will be offered to be included via the '+' button
  // on the right. See IncludeFilesListReady.
  FFindThread := TFileFindThread.Create;
  FFindThread.FileMasks.Add('*.inc');
  FFindThread.SearchDirs.Assign(FSearchPath);
  FFindThread.OnFindComplete := IncludeFilesListReady;
  FFindThread.StartFind;

  InitCompilerVersion;
  InitRtlVersion;
  InitVerXxx;
  InitOptions;
  InitIncludes;

  // tabheight = pc_IfClasses.Height - pc_IfClasses.TabRect.height - tabsheet1.borderwidth;
  r := pc_IfClasses.TabRect(0);
  b_Add.Height := r.Bottom - r.Top;
  b_Add.Top := r.Top;
  b_Add.Left := pc_IfClasses.Width - b_Add.Width;

  pc_IfClassesChange(pc_IfClasses);
end;

destructor TfmConfigureIfDef.Destroy;
begin
  if Assigned(FFindThread) then begin
    FFindThread.OnFindComplete := nil;
    FFindThread.Terminate;
  end;
  FreeAndNil(FFindThread);
  FreeAndNil(FSearchPath);
  FreeAndNil(FTabDefinitions);
  inherited;
end;

{ TIfdefTabDefinition }

type
  PGridEntry = ^TGridEntry;
  TGridEntry = record
    Value: string;
    Desc: string;
    LineNo: Integer;
  end;

type
  TIfdefTabDefinition = class
  protected
    FForm: TfmConfigureIfDef;
    FTabSheet: TTabSheet;
    FFilterEdit: TEdit;
    FStringGrid: TStringGrid;
    FCustomizeEdit: TEdit;
    FSelStart: Integer;
    FSelLen: Integer;
    FTextFormatStr: string;
    FFilename: string;
    FIsIncluded: Boolean;
    FGridEntries: array of TGridEntry;
    procedure AddEntryToGrid(const _Entry: TGridEntry);
    procedure UpdateEditText(_Row: Integer);
    procedure HandleFilterChange(_Sender: TObject);
    procedure HandleFilterKeyDown(_Sender: TObject; var _Key: Word; _Shift: TShiftState);
    procedure HandleCustomizeKeyDown(_Sender: TObject; var _Key: Word; _Shift: TShiftState);
    procedure HandleSelectCell(_Sender: TObject; _Col, _Row: Integer; var _CanSelect: Boolean);
    procedure HandleCustomizeEnter(_Sender: TObject);
    procedure HandleCustomizeChange(_Sender: TObject);
  public
    constructor Create(_Form: TfmConfigureIfDef; _pc: TPageControl; const _Caption: string;
      _SelStart, _SelLen: Integer; const _TextFormatStr: string;
      _IsIncluded: Boolean = True; const _Filename: string = '');
    procedure InitEvents;
    procedure AddEntry(const _Value, _Desc: string; _LineNo: Integer = 0);
    function GetLineNoOfCurrentEntry: Integer;
    property Filename: string read FFilename;
    property StringGrid: TStringGrid read FStringGrid;
    property Edit: TEdit read FCustomizeEdit;
    property IsIncluded: Boolean read FIsIncluded;
  end;

constructor TIfdefTabDefinition.Create(_Form: TfmConfigureIfDef; _pc: TPageControl;
  const _Caption: string; _SelStart, _SelLen: Integer; const _TextFormatStr: string;
  _IsIncluded: Boolean = True; const _Filename: string = '');
var
  lbl: TLabel;
  pnl: TPanel;
begin
  FForm := _Form;
  FSelStart := _SelStart;
  FSelLen := _SelLen;
  FTextFormatStr := _TextFormatStr;
  FFilename := _Filename;
  FIsIncluded := _IsIncluded;

  FTabSheet := TTabSheet.Create(_Form);
  FTabSheet.Name := '';
  FTabSheet.Parent := _pc;
  FTabSheet.PageControl := _pc;
  FTabSheet.Caption := _Caption + ' ';
  if FFilename <> '' then
    FTabSheet.Hint := FFilename;

  // this is how the PageControl knows how to deal with this tab
  FTabSheet.Tag := Integer(Self);

  pnl := TPanel.Create(_Form);
  pnl.Name := '';
  pnl.Parent := FTabSheet;
  pnl.Height := 25;
  pnl.Align := alTop;
  pnl.Caption := '';
  pnl.TabOrder := 0;

  lbl := TLabel.Create(_Form);
  lbl.Name := '';
  lbl.Parent := pnl;
  lbl.Caption := '&Filter: ';
  lbl.Align := alLeft;

  FFilterEdit := TEdit.Create(_Form);
  FFilterEdit.Name := '';
  FFilterEdit.Parent := pnl;
  FFilterEdit.Align := alClient;
  lbl.FocusControl := FFilterEdit;

  FStringGrid := TStringGrid.Create(_Form);
  FStringGrid.Name := '';
  FStringGrid.Parent := FTabSheet;
  FStringGrid.Align := alClient;
  FStringGrid.ColCount := 2;
  FStringGrid.FixedCols := 0;
  FStringGrid.FixedRows := 0;
  FStringGrid.Options := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSelect];
  FStringGrid.TabOrder := 1;
  FStringGrid.ColWidths[0] := 50;
  FStringGrid.ColWidths[1] := 200;
  GxOtaGetEditorFont(FStringGrid.Font, 0);

  pnl := TPanel.Create(_Form);
  pnl.Name := '';
  pnl.Parent := FTabSheet;
  pnl.Align := alBottom;
  pnl.BevelOuter := bvNone;
  pnl.Height := 25;
  pnl.TabOrder := 2;

  lbl := TLabel.Create(_Form);
  lbl.Name := '';
  lbl.Parent := pnl;
  lbl.Caption := 'C&ustomize: ';
  lbl.Align := alLeft;

  FCustomizeEdit := TEdit.Create(_Form);
  FCustomizeEdit.Name := '';
  FCustomizeEdit.Parent := pnl;
  FCustomizeEdit.Align := alClient;
  FCustomizeEdit.TabOrder := 0;
  lbl.FocusControl := FCustomizeEdit;
end;

function TIfdefTabDefinition.GetLineNoOfCurrentEntry: Integer;
var
  r: Integer;
begin
  r := FStringGrid.Row;
  if (r >= FStringGrid.FixedRows) and (r < FStringGrid.RowCount) then
    Result := GXNativeInt(FStringGrid.Objects[0, r])
  else
    Result := 0;
end;

procedure TIfdefTabDefinition.InitEvents;
begin
  TStringGrid_AdjustRowHeight(FStringGrid);
  TGrid_Resize(FStringGrid, [roUseGridWidth, roUseAllRows]);
  FStringGrid.OnSelectCell := HandleSelectCell;

  FFilterEdit.OnChange := HandleFilterChange;
  FFilterEdit.OnKeyDown := HandleFilterKeyDown;

  FCustomizeEdit.OnChange := HandleCustomizeChange;
  FCustomizeEdit.OnEnter := HandleCustomizeEnter;
  FCustomizeEdit.OnKeyDown := HandleCustomizeKeyDown;

  UpdateEditText(FStringGrid.Row);
end;

procedure TIfdefTabDefinition.AddEntry(const _Value, _Desc: string; _LineNo: Integer);
var
  Idx: Integer;
  Entry: PGridEntry;
begin
  Idx := Length(FGridEntries);
  SetLength(FGridEntries, Idx + 1);
  Entry := @FGridEntries[Idx];
  Entry^.Value := _Value;
  Entry^.Desc := _Desc;
  Entry^.LineNo := _LineNo;

  AddEntryToGrid(Entry^);
end;

procedure TIfdefTabDefinition.AddEntryToGrid(const _Entry: TGridEntry);
var
  Row: Integer;
begin
  Row := TStringGrid_AppendRow(FStringGrid, [_Entry.Value, _Entry.Desc], True);
  FStringGrid.Objects[0, Row] := Pointer(_Entry.LineNo);
end;

procedure TIfdefTabDefinition.UpdateEditText(_Row: Integer);
var
  CellText: string;
  SelText: string;
  NewText: string;
begin
  CellText := FStringGrid.Cells[0, _Row];

  SelText := FCustomizeEdit.SelText;
  NewText := Format(FTextFormatStr, [CellText]);
  if FForm.chk_AppendComment.Checked then begin
    CellText := FStringGrid.Cells[1, _Row];
    NewText := NewText + ' // ' + CellText;
  end;
  FCustomizeEdit.Text := NewText;
  FCustomizeEdit.SelStart := FSelStart;
  FCustomizeEdit.SelLength := FSelLen;
  if SelText <> '' then begin
    FCustomizeEdit.SelText := SelText;
    FCustomizeEdit.SelStart := FSelStart;
    FCustomizeEdit.SelLength := FSelLen;
  end;
end;

procedure TIfdefTabDefinition.HandleSelectCell(_Sender: TObject; _Col, _Row: Integer;
  var _CanSelect: Boolean);
begin
  UpdateEditText(_Row);
end;

procedure TIfdefTabDefinition.HandleCustomizeChange(_Sender: TObject);
begin
  FForm.FText := FCustomizeEdit.Text;
end;

procedure TIfdefTabDefinition.HandleFilterChange(_Sender: TObject);
var
  Filter: string;
  i: Integer;
  Entry: PGridEntry;
begin
  Filter := FFilterEdit.Text;
  TStringGrid_Clear(FStringGrid);
  if Filter = '' then begin
    for i := 0 to Length(FGridEntries) - 1 do
      AddEntryToGrid(FGridEntries[i]);
  end else begin
    for i := 0 to Length(FGridEntries) - 1 do begin
      Entry := @FGridEntries[i];
      if StrContains(Filter, Entry^.Value, False) or StrContains(Filter, Entry.Desc, False) then
        AddEntryToGrid(Entry^);
    end;
  end;
end;

procedure TIfdefTabDefinition.HandleFilterKeyDown(_Sender: TObject; var _Key: Word; _Shift: TShiftState);
begin
  if (_Key in [VK_DOWN, VK_UP, VK_NEXT, VK_PRIOR]) then begin
    FStringGrid.Perform(WM_KEYDOWN, _Key, 0);
    _Key := 0;
  end;
end;

procedure TIfdefTabDefinition.HandleCustomizeEnter(_Sender: TObject);
begin
  FCustomizeEdit.SelStart := FSelStart;
  FCustomizeEdit.SelLength := FSelLen;
end;

procedure TIfdefTabDefinition.HandleCustomizeKeyDown(_Sender: TObject; var _Key: Word; _Shift: TShiftState);
begin
  if (_Key in [VK_DOWN, VK_UP, VK_NEXT, VK_PRIOR]) then begin
    FStringGrid.Perform(WM_KEYDOWN, _Key, 0);
    _Key := 0;
  end;
end;

procedure TfmConfigureIfDef.pc_IfClassesChange(Sender: TObject);
var
  ts: TTabSheet;
  def: TIfdefTabDefinition;
begin
  ts := pc_IfClasses.ActivePage;
  if ts.Tag <> 0 then begin
    def := TIfdefTabDefinition(ts.Tag);
    act_Open.Enabled := (def.Filename <> '');
    TGrid_Resize(def.StringGrid, [roUseGridWidth, roUseAllRows]);
    TWinControl_SetFocus(def.Edit);
    FText := def.Edit.Text;
  end;
end;

function TfmConfigureIfDef.GetIncludeFile: string;
var
  ts: TTabSheet;
  def: TIfdefTabDefinition;
begin
  Result := '';
  ts := pc_IfClasses.ActivePage;
  if ts.Tag <> 0 then begin
    def := TIfdefTabDefinition(ts.Tag);
    if not def.IsIncluded then
      Result := ExtractFileName(def.Filename);
  end;
end;

procedure TfmConfigureIfDef.pc_IfClassesMouseLeave(Sender: TObject);
begin
  pc_IfClasses.Hint := '';
  pc_IfClasses.ShowHint := False;
end;

procedure TfmConfigureIfDef.pc_IfClassesMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  hintPause: Integer;
  TabIndex: Integer;
begin
  TabIndex := pc_IfClasses.IndexOfTabAt(X, Y);
  if (TabIndex >= 0) and (pc_IfClasses.Hint <> pc_IfClasses.Pages[TabIndex].Hint) then begin
    hintPause := Application.hintPause;
    try
      if pc_IfClasses.Hint <> '' then
        Application.hintPause := 0;
      Application.CancelHint;
      pc_IfClasses.Hint := pc_IfClasses.Pages[TabIndex].Hint;
      pc_IfClasses.ShowHint := True;
      Application.ProcessMessages; // force hint to appear
    finally
      Application.hintPause := hintPause;
    end;
  end;
end;

procedure TfmConfigureIfDef.act_AddExecute(Sender: TObject);
var
  pnt: TPoint;
begin
  pnt := Point(0, b_Add.Height);
  pnt := b_Add.ClientToScreen(pnt);
  pm_IncludeFiles.Popup(pnt.X, pnt.Y);
end;

procedure TfmConfigureIfDef.act_OpenExecute(Sender: TObject);
var
  ts: TTabSheet;
  def: TIfdefTabDefinition;
  LineNo: Integer;
begin
  ts := pc_IfClasses.ActivePage;
  if ts.Tag <> 0 then begin
    def := TIfdefTabDefinition(ts.Tag);
    if def.Filename = '' then
      Exit;

    LineNo := def.GetLineNoOfCurrentEntry;
    GxOtaGoToFileLine(def.Filename, LineNo);
    ModalResult := mrCancel;
  end;
end;

procedure TfmConfigureIfDef.chk_AppendCommentClick(Sender: TObject);
var
  ts: TTabSheet;
  def: TIfdefTabDefinition;
begin
  pc_IfClassesChange(nil);
  ts := pc_IfClasses.ActivePage;
  if ts.Tag <> 0 then begin
    def := TIfdefTabDefinition(ts.Tag);
    def.UpdateEditText(def.StringGrid.Row);
  end;
end;

procedure TfmConfigureIfDef.InitOptions;
var
  def: TIfdefTabDefinition;
begin
  def := TIfdefTabDefinition.Create(Self, pc_IfClasses, '&Options', 9, 1, '{$IFOPT %s+}');
  FTabDefinitions.Add(def);
  def.AddEntry('A', 'Align');
  def.AddEntry('B', 'BoolEval');
  def.AddEntry('C', 'Assertions');
  def.AddEntry('D', 'DebugInfo');
  def.AddEntry('E', '???');
  def.AddEntry('F', '???');
  def.AddEntry('G', 'ImportedData');
  def.AddEntry('H', 'LongStrings');
  def.AddEntry('I', 'IoChecks');
  def.AddEntry('J', 'WriteableConsts');
  def.AddEntry('K', '???');
  def.AddEntry('L', 'LocalSymbols');
  def.AddEntry('M', 'TypeInfo');
  def.AddEntry('N', '???');
  def.AddEntry('O', 'Optimization');
  def.AddEntry('P', 'OpenStrings');
  def.AddEntry('Q', 'OverflowChecks');
  def.AddEntry('R', 'RangeChecks');
  def.AddEntry('S', '???');
  def.AddEntry('T', 'TypedAddress');
  def.AddEntry('U', 'SafeDivide');
  def.AddEntry('V', 'VarStringChecks');
  def.AddEntry('W', 'StackFrames');
  def.AddEntry('X', 'ExtendedSyntax');
  def.AddEntry('Y', 'ReferenceInfo / DefinitionInfo');
  def.AddEntry('Z', '???');
  def.InitEvents;
end;

procedure TfmConfigureIfDef.InitVerXxx;
var
  def: TIfdefTabDefinition;
begin
{$IFDEF VER350}
{$MESSAGE HINT 'Add a new Delphi version here'}
{$ENDIF}
  def := TIfdefTabDefinition.Create(Self, pc_IfClasses, '&VERxxx', 4, 1, '{$IFNDEF %s}');
  FTabDefinitions.Add(def);
  def.AddEntry('VER340', 'Delphi 10.4 Sydney / BDS 21');
  def.AddEntry('VER330', 'Delphi 10.3 Rio / BDS 20');
  def.AddEntry('VER320', 'Delphi 10.2 Tokyo / BDS 19');
  def.AddEntry('VER310', 'Delphi 10.1 Berlin / BDS 18');
  def.AddEntry('VER300', 'Delphi 10.0 Seattle / BDS 17');
  def.AddEntry('VER290', 'Delphi XE8 / BDS 16');
  def.AddEntry('VER280', 'Delphi XE7 / BDS 15');
  def.AddEntry('VER270', 'Delphi XE6 / BDS 14');
  def.AddEntry('VER265', 'AppMethod');
  def.AddEntry('VER260', 'Delphi XE5 / BDS 12');
  def.AddEntry('VER250', 'Delphi XE4 / BDS 11');
  def.AddEntry('VER240', 'Delphi XE3 / BDS 10');
  def.AddEntry('VER230', 'Delphi XE2 / BDS 9');
  def.AddEntry('VER220', 'Delphi XE1 / BDS 8');
  def.AddEntry('VER210', 'Delphi 2010 / BDS 7');
  def.AddEntry('VER200', 'Delphi 2009 / BDS 6');
  def.AddEntry('VER190', 'Delphi 2007 .NET');
  def.AddEntry('VER185', 'Delphi 2007 / BDS 4');
  def.AddEntry('VER180', 'Delphi 2006/2007 / BDS 3');
  def.AddEntry('VER170', 'Delphi 2005 / BDS 2');
  def.AddEntry('VER160', 'Delphi 8 .NET / BDS 1');
  def.AddEntry('VER150', 'Delphi 7');
  def.AddEntry('VER140', 'Delphi 6');
  def.AddEntry('VER130', 'Delphi 5');
  def.AddEntry('VER120', 'Delphi 4');
  def.AddEntry('VER100', 'Delphi 3');
  def.AddEntry('VER90', 'Delphi 2');
  def.AddEntry('VER80', 'Delphi 1');
  def.InitEvents;
end;

procedure TfmConfigureIfDef.InitRtlVersion;
var
  def: TIfdefTabDefinition;
begin
{$IF RTLVersion > RtlVersionDelphiSydney}
{$MESSAGE HINT 'Add a new Delphi version here'}
{$IFEND}
  def := TIfdefTabDefinition.Create(Self, pc_IfClasses, '&RtlVersion', 16, 2, '{$IF RtlVersion >= %s}');
  FTabDefinitions.Add(def);
  def.AddEntry(IntToStr(RtlVersionDelphiSydney), 'Delphi 10.4 Sydney / BDS 21');
  def.AddEntry(IntToStr(RtlVersionDelphiRio), 'Delphi 10.3 Rio / BDS 20');
  def.AddEntry(IntToStr(RtlVersionDelphiTokyo), 'Delphi 10.2 Tokyo / BDS 19');
  def.AddEntry(IntToStr(RtlVersionDelphiBerlin), 'Delphi 10.1 Berlin / BDS 18');
  def.AddEntry(IntToStr(RtlVersionDelphiSeattle), 'Delphi 10.0 Seattle / BDS 17');
  def.AddEntry(IntToStr(RtlVersionDelphixe8), 'Delphi XE8 / BDS 16');
  def.AddEntry(IntToStr(RtlVersionDelphiXE7), 'Delphi XE7 / BDS 15');
  def.AddEntry(IntToStr(RtlVersionDelphiXE6), 'Delphi XE6 / BDS 14');
//  def.AddEntry('26.5', 'AppMethod'); ???
  def.AddEntry(IntToStr(RtlVersionDelphiXE5), 'Delphi XE5 / BDS 12');
  def.AddEntry(IntToStr(RtlVersionDelphiXE4), 'Delphi XE4 / BDS 11');
  def.AddEntry(IntToStr(RtlVersionDelphiXE3), 'Delphi XE3 / BDS 10');
  def.AddEntry(IntToStr(RtlVersionDelphiXE2), 'Delphi XE2 / BDS 9');
  def.AddEntry(IntToStr(RtlVersionDelphiXE), 'Delphi XE1 / BDS 8');
  def.AddEntry(IntToStr(RtlVersionDelphi2010), 'Delphi 2010 / BDS 7');
  def.AddEntry(IntToStr(RtlVersionDelphi2009), 'Delphi 2009 / BDS 6');
//  def.AddEntry('19', 'Delphi 2007 .NET'); ???
  def.AddEntry(IntToStr(RtlVersionDelphi2007), 'Delphi 2007 / BDS 4');
  def.AddEntry(IntToStr(RtlVersionDelphi2006), 'Delphi 2006 / BDS 3');
  def.AddEntry(IntToStr(RtlVersionDelphi2005), 'Delphi 2005 / BDS 2');
  def.AddEntry(IntToStr(RtlVersionDelphi8), 'Delphi 8 .NET / BDS 1');
  def.AddEntry(IntToStr(RtlVersionDelphi7), 'Delphi 7');
  def.AddEntry(IntToStr(RtlVersionDelphi6), 'Delphi 6');
  def.InitEvents;
end;

procedure TfmConfigureIfDef.InitCompilerVersion;
var
  def: TIfdefTabDefinition;
begin
  def := TIfdefTabDefinition.Create(Self, pc_IfClasses, '&CompilerVersion', 21, 2, '{$IF CompilerVersion >= %s}');
  FTabDefinitions.Add(def);
{$IF CompilerVersion > CompilerVersionDelphiSydney}
{$MESSAGE HINT 'Add a new Delphi version here'}
{$IFEND}
  def.AddEntry(IntToStr(CompilerVersionDelphiSydney), 'Delphi 10.4 Sydney / BDS 21');
  def.AddEntry(IntToStr(CompilerVersionDelphiRio), 'Delphi 10.3 Rio / BDS 20');
  def.AddEntry(IntToStr(CompilerVersionDelphiTokyo), 'Delphi 10.2 Tokyo / BDS 19');
  def.AddEntry(IntToStr(CompilerVersionDelphiBerlin), 'Delphi 10.1 Berlin / BDS 18');
  def.AddEntry(IntToStr(CompilerVersionDelphiSeattle), 'Delphi 10.0 Seattle / BDS 17');
  def.AddEntry(IntToStr(CompilerVersionDelphiXE8), 'Delphi XE8 / BDS 16');
  def.AddEntry(IntToStr(CompilerVersionDelphiXE7), 'Delphi XE7 / BDS 15');
  def.AddEntry(IntToStr(CompilerVersionDelphiXE6), 'Delphi XE6 / BDS 14');
  def.AddEntry('26.5', 'AppMethod');
  def.AddEntry(IntToStr(CompilerVersionDelphiXE5), 'Delphi XE5 / BDS 12');
  def.AddEntry(IntToStr(CompilerVersionDelphiXE4), 'Delphi XE4 / BDS 11');
  def.AddEntry(IntToStr(CompilerVersionDelphiXE3), 'Delphi XE3 / BDS 10');
  def.AddEntry(IntToStr(CompilerVersionDelphiXE2), 'Delphi XE2 / BDS 9');
  def.AddEntry(IntToStr(CompilerVersionDelphiXE), 'Delphi XE1 / BDS 8');
  def.AddEntry(IntToStr(CompilerVersionDelphi2010), 'Delphi 2010 / BDS 7');
  def.AddEntry(IntToStr(CompilerVersionDelphi2009), 'Delphi 2009 / BDS 6');
  def.AddEntry(Format('%.1f', [CompilerVersionDelphi2007]), 'Delphi 2007 / BDS 4');
  def.AddEntry(IntToStr(CompilerVersionDelphi2007Net), 'Delphi 2007.NET');
  def.AddEntry(IntToStr(CompilerVersionDelphi2006), 'Delphi 2006 / BDS 3');
  def.AddEntry(IntToStr(CompilerVersionDelphi2005), 'Delphi 2005 / BDS 2');
  def.AddEntry(IntToStr(CompilerVersionDelphi8), 'Delphi 8 .NET / BDS 1');
  def.AddEntry(IntToStr(CompilerVersionDelphi7), 'Delphi 7');
  def.AddEntry(IntToStr(CompilerVersionDelphi6), 'Delphi 6');
  def.InitEvents;
end;

type
  TIncDirective = class
  private
    FLineIdx: Integer;
    FComment: string;
  public
    constructor Create(_LineIdx: Integer; const _Comment: string);
    property LineIdx: Integer read FLineIdx;
    property Comment: string read FComment;
  end;

function IsCompilerDirective(const _Line: string; const _Directive: string;
  out _Value: string; out _Comment: string): Boolean;
var
  Incl: string;
  s: string;
  p: Integer;
begin
  _Comment := '';
  Incl := '{' + _Directive + ' ';
  s := Trim(_Line);
  Result := StartsText(Incl, s);
  if Result then begin
    p := Pos('}', s);
    if p > Length(Incl) then begin
      _Value := Copy(s, Length(Incl) + 1, p - 1 - Length(Incl));
      _Value := Trim(_Value);
      _Value := AnsiDequotedStr(_Value, '''');
      s := Copy(s, p + 1);
      p := Pos('//', s);
      if p > 0 then
        _Comment := Trim(Copy(s, p + 2));
    end;
  end;
end;

function TfmConfigureIfDef.AddIncludePage(_No: Integer; const _FullFn: string; _IsIncluded: Boolean): TTabSheet;
const
  DEFINE_STR = '$DEFINE';
  UNDEF_STR = '$UNDEF';
  NOT_DEFINE_STR = '.$DEFINE';
  NOT_UNDEF_STR = '.$UNDEF';
var
  def: TIfdefTabDefinition;
  Lines: TGXUnicodeStringList;
  LineIdx: Integer;
  Line: string;
  Define: string;
  Comment: string;
  Directives: TStringList;
  Idx: Integer;
  id: TIncDirective;
begin
  Result := nil;
  Directives := nil;
  Lines := TGXUnicodeStringList.Create;
  try
    Directives := TStringList.Create;
    Directives.Sorted := True;
    Lines.LoadFromFile(_FullFn);
    for LineIdx := 0 to Lines.Count - 1 do begin
      Line := Lines[LineIdx];
      Line := Trim(Line);
      if IsCompilerDirective(Line, DEFINE_STR, Define, Comment)
        or IsCompilerDirective(Line, UNDEF_STR, Define, Comment)
        or IsCompilerDirective(Line, NOT_DEFINE_STR, Define, Comment)
        or IsCompilerDirective(Line, NOT_UNDEF_STR, Define, Comment) then begin
        if not Directives.Find(Define, Idx) then begin
          Idx := Directives.Add(Define);
          Directives.Objects[Idx] := TIncDirective.Create(LineIdx, Comment);
        end;
      end;
    end;
    if Directives.Count > 0 then begin
      def := TIfdefTabDefinition.Create(Self, pc_IfClasses,
        Format('&%d %s', [_No, ExtractFileName(_FullFn)]), 4, 1, '{$IFNDEF %s}', _IsIncluded, _FullFn);
      FTabDefinitions.Add(def);
      for Idx := 0 to Directives.Count - 1 do begin
        Define := Directives[Idx];
        id := TIncDirective(Directives.Objects[Idx]);
        def.AddEntry(Define, id.Comment, id.LineIdx + 1);
        FreeAndNil(id);
      end;
      def.InitEvents;
      Result := def.FTabSheet;
    end;
  finally
    FreeAndNil(Directives);
    FreeAndNil(Lines);
  end;
end;

procedure TfmConfigureIfDef.InitIncludes;

  procedure AddIfFound(var _Number: Integer; const _fn: string);
  var
    i: Integer;
    FullFn: string;
  begin
    for i := 0 to FSearchPath.Count - 1 do begin
      FullFn := AddSlash(FSearchPath[i]) + _fn;
      if FileExists(FullFn) then begin
        if not IsKnownSourceFile(FullFn) then begin
          AddIncludePage(_Number, FullFn, True);
          Inc(_Number);
        end;
      end;
    end;
  end;

  procedure HandleFile(var _Number: Integer; _Lines: TGXUnicodeStringList);
  const
    INCLUDE_STR = '$Include';
    I_STR = '$I';
  var
    LineIdx: Integer;
    s: string;
    fn: string;
    Comment: string;
  begin
    for LineIdx := 0 to _Lines.Count - 1 do begin
      s := _Lines[LineIdx];
      if IsCompilerDirective(s, INCLUDE_STR, fn, Comment)
        or IsCompilerDirective(s, I_STR, fn, Comment) then begin
        AddIfFound(_Number, fn);
      end;
    end;
  end;

var
  Number: Integer;
  Lines: TGXUnicodeStringList;
  DefIdx: Integer;
  def: TIfdefTabDefinition;
begin
  Number := 1;
  Lines := TGXUnicodeStringList.Create;
  try
    if not GxOtaGetActiveEditorText(Lines, False) then
      Exit;

    HandleFile(Number, Lines);
  finally
    FreeAndNil(Lines);
  end;

  // search those include files for additional includes
  // todo: Maybe these should just be added to the defines of the original include files
  // rather than to a separate tab. The user hasn't explicitly included them after all
  // and might not even be aware of them.
  DefIdx := 0;
  while DefIdx < FTabDefinitions.Count do begin
    def := FTabDefinitions[DefIdx] as TIfdefTabDefinition;
    if def.Filename <> '' then begin
      Lines := TGXUnicodeStringList.Create;
      try
        Lines.LoadFromFile(def.Filename);
        HandleFile(Number, Lines);
      finally
        FreeAndNil(Lines);
      end;
    end;
    Inc(DefIdx);
  end;
end;

function TfmConfigureIfDef.IsKnownIncFile(const _fn: string): Boolean;
var
  DefIdx: Integer;
  def: TIfdefTabDefinition;
begin
  for DefIdx := 0 to FTabDefinitions.Count - 1 do begin
    def := FTabDefinitions[DefIdx] as TIfdefTabDefinition;
    if SameText(_fn, def.Filename) then begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

procedure TfmConfigureIfDef.IncludeFilesListReady;
var
  ResIdx: Integer;
  fn: string;
begin
  // This is called via Synchronize.
  // Can we be sure that the static tabs have already been created?
  if not Assigned(FFindThread) then begin
    // Ff the form's constructor raises an exception after the thread has been
    // started, FFindThread might will already have been freed here because it
    // was waiting in Synchronize for the main thread to become idle.
    // Checking for FFindThread = NIL is not an ideal solution for this, but
    // still better than an Access Violation.
    Exit; //==>
  end;

  for ResIdx := 0 to FFindThread.Results.Count - 1 do begin
    fn := FFindThread.Results[ResIdx];
    if not IsKnownIncFile(fn) then begin
      TPopupMenu_AppendMenuItem(pm_IncludeFiles, fn, OnInludeFileSelected);
    end;
  end;
end;

procedure TfmConfigureIfDef.OnInludeFileSelected(_Sender: TObject);
var
  DefIdx: Integer;
  def: TIfdefTabDefinition;
  Number: Integer;
  mi: TMenuItem;
  ts: TTabSheet;
begin
  mi := _Sender as TMenuItem;
  Number := 1;
  for DefIdx := 0 to FTabDefinitions.Count - 1 do begin
    def := FTabDefinitions[DefIdx] as TIfdefTabDefinition;
    if def.Filename <> '' then
      Inc(Number);
  end;
  ts := AddIncludePage(Number, StripHotkey(mi.Caption), False);
  if Assigned(ts) then
    pc_IfClasses.ActivePage := ts;
  mi.Free;
end;

{ TIncDirective }

constructor TIncDirective.Create(_LineIdx: Integer; const _Comment: string);
begin
  inherited Create;
  FLineIdx := _LineIdx;
  FComment := _Comment;
end;

{$IF RTLVersion <= RtlVersionDelphi2005}

{ TPageControl }

procedure TPageControl.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  mEvnt: TTrackMouseEvent;
begin
  inherited;
  if not FMouseTracking then begin
    mEvnt.cbSize := SizeOf(mEvnt);
    mEvnt.dwFlags := TME_LEAVE;
    mEvnt.hwndTrack := Handle;
    TrackMouseEvent(mEvnt);
    FMouseTracking := True;
  end;
end;

procedure TPageControl.WMMouseLeave(var Msg: TMessage);
begin
  Msg.Result := 0;
  FMouseTracking := False;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
{$IFEND}

initialization
  RegisterEditorExpert(TIfDefExpert);
end.
