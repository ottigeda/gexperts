unit GX_ePopupMenu;

{$I GX_CondDefine.inc}

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
  ComCtrls,
  StdCtrls,
  Menus,
  GX_BaseForm,
  GX_BaseExpert;

type
  TfmEditorPopupMenuExpertConfig = class(TfmBaseForm)
    pc_Main: TPageControl;
    ts_Experts: TTabSheet;
    ts_EditorExperts: TTabSheet;
    lb_EditorExperts: TListBox;
    lb_Experts: TListBox;
    lv_Selected: TListView;
    b_Ok: TButton;
    b_Cancel: TButton;
    b_Add: TButton;
    b_Remove: TButton;
    l_DuplicateShortcuts: TLabel;
    b_Default: TButton;
    b_ClearShortcut: TButton;
    chk_FocusEditor: TCheckBox;
    pm_Selected: TPopupMenu;
    procedure b_AddClick(Sender: TObject);
    procedure b_RemoveClick(Sender: TObject);
    procedure lb_EditorExpertsDblClick(Sender: TObject);
    procedure lb_ExpertsDblClick(Sender: TObject);
    procedure lv_SelectedEditing(Sender: TObject; Item: TListItem; var AllowEdit: Boolean);
    procedure lv_SelectedEdited(Sender: TObject; Item: TListItem; var s: string);
    procedure lv_SelectedDblClick(Sender: TObject);
    procedure lv_SelectedChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure b_DefaultClick(Sender: TObject);
    procedure b_ClearShortcutClick(Sender: TObject);
    procedure pm_SelectedPopup(Sender: TObject);
  private
    FOldlvSelectWindowProc: TWndMethod;
    function GetExpertIndex(const _ListView: TListView; const _Expert: TGX_BaseExpert): Integer;
    procedure CheckForDuplicates;
    procedure GetData(_sl: TStringList; out _ForceEdiorActive: Boolean);
    procedure SetData(_sl: TStringList; _ForceEdiorActive: Boolean);
    procedure AddExpert(const _Key: string; const _ExpName: string; _Expert: TGX_BaseExpert);
    procedure RemoveExpert;
    procedure AddSelectedExpert;
    procedure AddSelectedEditorExpert;
    procedure EnableOKCancel;
    procedure mi_SetHotkey(_Sender: TObject);
    procedure mi_EditHotkey(_Sender: TObject);
    procedure ListViewWndProc(var _Message: TMessage);
  public
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  StrUtils,
  CommCtrl,
  GX_EditorExpert,
  GX_ConfigurationInfo,
  GX_dzVclUtils,
  GX_GExperts,
  GX_EditorExpertManager,
  GX_Experts,
  GX_MessageBox,
  GX_OtaUtils,
  GX_MenusForEditorExpert;

type
  TGxEditorPopupMenuExpert = class(TEditorExpert)
  private
    FFormHeight: Integer;
    FGExpertsShortcutMenu: TPopupMenu;
    FShortcuts: TStringList;
    FForceEditorActive: Boolean;
    procedure ShowConfigForm(_Sender: TObject);
    class procedure SetDefaults(_sl: TStringList);
    function IsEditorActive(out ctl: TWinControl): Boolean;
  protected
    function GetDisplayName: string; override;
    class function GetName: string; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Configure; override;
    procedure Execute(Sender: TObject); override;
    function GetDefaultShortCut: TShortCut; override;
    function GetHelpString: string; override;
    function HasConfigOptions: Boolean; override;
    procedure InternalLoadSettings(_Settings: IExpertSettings); override;
    procedure InternalSaveSettings(_Settings: IExpertSettings); override;
  end;

{ TGxEditorPopupMenuExpert }

procedure TGxEditorPopupMenuExpert.Configure;
var
  frm: TfmEditorPopupMenuExpertConfig;
begin
  frm := TfmEditorPopupMenuExpertConfig.Create(nil);
  try
    frm.SetData(FShortcuts, FForceEditorActive);
    frm.Height := FFormHeight;
    if frm.ShowModal <> mrOk then
      Exit;
    frm.GetData(FShortcuts, FForceEditorActive);
    FFormHeight := frm.Height;
  finally
    FreeAndNil(frm);
  end;
end;

constructor TGxEditorPopupMenuExpert.Create;
begin
  inherited;
  FShortcuts := TStringList.Create;
  FForceEditorActive := True;
end;

destructor TGxEditorPopupMenuExpert.Destroy;
begin
  FreeAndNil(FGExpertsShortcutMenu);
  FreeAndNil(FShortcuts);
  inherited;
end;

function TGxEditorPopupMenuExpert.IsEditorActive(out ctl: TWinControl): Boolean;
begin
  ctl := Screen.ActiveControl;
  Result := Assigned(ctl) and (ctl.Name = 'Editor') and ctl.ClassNameIs('TEditControl');
end;

procedure TGxEditorPopupMenuExpert.Execute(Sender: TObject);
var
  ctl: TWinControl;
  pnt: TPoint;
  i: Integer;
  Key: string;
  idx: Integer;
  ExpName: string;
  Expert: TGX_BaseExpert;
  EditorForm: TCustomForm;
  EditorControl: TComponent;
begin
  if not IsEditorActive(ctl) then begin
    if not FForceEditorActive then
      Exit; //==>

    EditorForm := GxOtaGetTopMostEditView.GetEditWindow.Form;
    if Assigned(EditorForm) then begin
      EditorControl := EditorForm.FindComponent('Editor');
      if Assigned(EditorControl) and EditorControl.ClassNameIs('TEditControl') then
        TWinControl_SetFocus(EditorControl as TWinControl);
    end;
  end;

  if not IsEditorActive(ctl) then
    Exit;

  if Assigned(FGExpertsShortcutMenu) then
    FreeAndNil(FGExpertsShortcutMenu);
  FGExpertsShortcutMenu := TPopupMenu.Create(nil);
  for i := 0 to FShortcuts.Count - 1 do begin
    Key := FShortcuts.Names[i];
    ExpName := FShortcuts.Values[Key];
    if (Key <> '') and (Key <> 'X') then begin
      if GExpertsInst.EditorExpertManager.FindExpert(ExpName, idx) then begin
        Expert := GExpertsInst.EditorExpertManager.EditorExpertList[idx];
        TPopupMenu_AppendMenuItem(FGExpertsShortcutMenu, '&' + Key + ' ' + Expert.GetDisplayName,
          Expert.Execute);
      end else if GExpertsInst.FindExpert(ExpName, idx) then begin
        Expert := GExpertsInst.ExpertList[idx];
        if Expert is TMenusForEditorExperts then begin
          TPopupMenu_AppendMenuItem(FGExpertsShortcutMenu, '&' + Key + ' ' + Expert.GetDisplayName,
            TMenusForEditorExperts(Expert).ShowPopup);
        end else begin
          TPopupMenu_AppendMenuItem(FGExpertsShortcutMenu, '&' + Key + ' ' + Expert.GetDisplayName,
            Expert.Execute);
        end;
      end;
    end;
  end;
  TPopupMenu_AppendMenuItem(FGExpertsShortcutMenu, '&' + 'X' + ' ' + 'Configure',
    ShowConfigForm);
  pnt := ctl.ClientToScreen(Point(0, 0));
  FGExpertsShortcutMenu.Popup(pnt.X, pnt.Y);

  IncCallCount;
end;

procedure TGxEditorPopupMenuExpert.ShowConfigForm(_Sender: TObject);
begin
  GExpertsInst.ShowConfigurationForm;
end;

function TGxEditorPopupMenuExpert.GetDefaultShortCut: TShortCut;
begin
  Result := scCtrl + Ord('H');
end;

function TGxEditorPopupMenuExpert.GetDisplayName: string;
resourcestring
  SDisplayName = 'Editor Popup Menu';
begin
  Result := SDisplayName;
end;

function TGxEditorPopupMenuExpert.GetHelpString: string;
resourcestring
  SGxEditorPopupMenuExpertHelp =
    'Adds a new shortcut to the editor that shows a configurable popup menu ' +
    'as an alternative to the GExperts main menu.';
begin
  Result := SGxEditorPopupMenuExpertHelp;
end;

class function TGxEditorPopupMenuExpert.GetName: string;
const
  SName = 'EditorPopupMenu';
begin
  Result := SName;
end;

function TGxEditorPopupMenuExpert.HasConfigOptions: Boolean;
begin
  Result := True;
end;

procedure TGxEditorPopupMenuExpert.InternalLoadSettings(_Settings: IExpertSettings);
begin
  inherited;
  _Settings.ReadBool('ForceEditorActive', FForceEditorActive);
  FShortcuts.Clear;
  if _Settings.SectionExists('menu') then begin
    _Settings.ReadSectionValues('menu', FShortcuts);
  end else begin
    SetDefaults(FShortcuts);
  end;
  FFormHeight := _Settings.ReadInteger('FormHeight', FFormHeight);
end;

procedure TGxEditorPopupMenuExpert.InternalSaveSettings(_Settings: IExpertSettings);
var
  i: Integer;
  s: string;
  MnuSettings: IExpertSettings;
begin
  inherited;
  _Settings.WriteBool('ForceEditorActive', FForceEditorActive);
  _Settings.EraseSection('menu');
  MnuSettings := _Settings.Subkey('menu');
  for i := 0 to FShortcuts.Count - 1 do begin
    s := FShortcuts.Names[i];
    MnuSettings.WriteString(s, FShortcuts.Values[s]);
  end;
  _Settings.WriteInteger('FormHeight', FFormHeight);
end;

class procedure TGxEditorPopupMenuExpert.SetDefaults(_sl: TStringList);
begin
  _sl.Add('A=Align');
  _sl.Add('B=BookmarksExpert');
  _sl.Add('C=ClassBrowser');
  _sl.Add('D=DateTime');
  _sl.Add('E=EditorExpertsMenu');
  _sl.Add('F=CodeFormatter');
  _sl.Add('G=GrepSearch');
  _sl.Add('H=ClipboardHistory');
  _sl.Add('I=SelectIdent');
  _sl.Add('J=SortLines');
  // K
  _sl.Add('L=CodeLibrarian');
  _sl.Add('M=MacroLibrary');
  // N
  _sl.Add('O=OpenFile');
  _sl.Add('P=ProcedureList');
  // Q
  _sl.Add('R=ReverseStatement');
  _sl.Add('S=MessageDialog');
  _sl.Add('T=MacroTemplates');
  _sl.Add('U=UsesClauseMgr');
  // V
  _sl.Add('W=WARN');
  _sl.Add('X=Configure');
  // Y
  // Z
end;

{ TfmEditorPopupMenuExpertConfig }

constructor TfmEditorPopupMenuExpertConfig.Create(_Owner: TComponent);
var
  i: Integer;
  EdExpManager: TGxEditorExpertManager;
  Expert: TGX_BaseExpert;
  mi: TMenuItem;
begin
  inherited;

  TControl_SetMinConstraints(Self);
  Self.Constraints.MaxWidth := Self.Width;

  for i := 0 to GExpertsInst.ExpertCount - 1 do begin
    Expert := GExpertsInst.ExpertList[i];
    lb_Experts.Items.AddObject(Expert.GetDisplayName, Expert);
  end;

  EdExpManager := GExpertsInst.EditorExpertManager;
  for i := 0 to EdExpManager.EditorExpertCount - 1 do begin
    Expert := EdExpManager.EditorExpertList[i];
    if not (Expert is TGxEditorPopupMenuExpert) then
      lb_EditorExperts.Items.AddObject(Expert.GetDisplayName, Expert);
  end;
  lb_EditorExperts.Sorted := True;

  mi := TPopupMenu_AppendMenuItem(pm_Selected, 'Edit Hotkey', mi_EditHotkey);
  mi.ShortCut := ShortCut(VK_F2, []);

  FOldlvSelectWindowProc := lv_Selected.WindowProc;
  lv_Selected.WindowProc := ListViewWndProc;
end;

procedure TfmEditorPopupMenuExpertConfig.ListViewWndProc(var _Message: TMessage);
var
  LvDispInfo: PLVDispInfo;
  Code: Integer;
begin
  if _Message.Msg = CN_NOTIFY then begin
    Code := PNMHdr(_Message.lParam).Code;
    if (Code = LVN_ENDLABELEDITA) or (Code = LVN_ENDLABELEDITW) then begin
      LvDispInfo := PLVDispInfo(_Message.lParam);
      if LvDispInfo.Item.pszText = nil then begin
        EnableOKCancel;
        Exit; //==>
      end;
    end;
  end;
  FOldlvSelectWindowProc(_Message);
end;

procedure TfmEditorPopupMenuExpertConfig.EnableOKCancel;
begin
  b_Ok.Enabled := True;
  b_Cancel.Enabled := True;
end;

procedure TfmEditorPopupMenuExpertConfig.AddSelectedEditorExpert;
var
  ExpName: string;
  Ex: TEditorExpert;
begin
  TListBox_GetSelectedObject(lb_EditorExperts, Pointer(Ex));
  ExpName := Ex.GetName;
  AddExpert('', ExpName, Ex);
end;

procedure TfmEditorPopupMenuExpertConfig.AddSelectedExpert;
var
  ExpName: string;
  Ex: TGX_Expert;
begin
  TListBox_GetSelectedObject(lb_Experts, Pointer(Ex));
  ExpName := Ex.GetName;
  AddExpert('', ExpName, Ex);
end;

procedure TfmEditorPopupMenuExpertConfig.b_AddClick(Sender: TObject);
begin
  if pc_Main.ActivePage = ts_Experts then begin
    AddSelectedExpert;
  end else begin
    AddSelectedEditorExpert;
  end;
end;

// ---------------------------------------------

type
  TClearIndividualShortcutMessage = class(TGxQuestionBoxAdaptor)
  protected
    function GetMessage: string; override;
  end;

{ TClearIndividualShortcutMessage }

function TClearIndividualShortcutMessage.GetMessage: string;
resourcestring
  SClearIndividualShortcut =
    'This will remove the shortcut assigned to the individual expert. So this expert can ' +
    'only be called via the GExperts main menu and via this editor menu. ' +
    'Do you want to clear the shortcut?';
begin
  Result := SClearIndividualShortcut;
end;

procedure TfmEditorPopupMenuExpertConfig.b_ClearShortcutClick(Sender: TObject);
var
  li: TListItem;
  Ex: TGX_BaseExpert;
begin
  if ShowGxMessageBox(TClearIndividualShortcutMessage) <> mrYes then
    Exit;
  if not TListView_TryGetSelected(lv_Selected, li) then
    Exit;
  Ex := TGX_BaseExpert(li.Data);
  Ex.ShortCut := 0
end;

procedure TfmEditorPopupMenuExpertConfig.b_RemoveClick(Sender: TObject);
begin
  RemoveExpert;
end;

procedure TfmEditorPopupMenuExpertConfig.b_DefaultClick(Sender: TObject);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    TGxEditorPopupMenuExpert.SetDefaults(sl);
    SetData(sl, True);
  finally
    FreeAndNil(sl);
  end;
end;

procedure TfmEditorPopupMenuExpertConfig.CheckForDuplicates;
var
  i: Integer;
  sl: TStringList;
  DupeFound: Boolean;
  Dummy: Boolean;
begin
  sl := TStringList.Create;
  try
    GetData(sl, Dummy);
    sl.Sort;
    DupeFound := False;
    for i := 1 to sl.Count - 1 do
      if sl.Names[i] = sl.Names[i - 1] then begin
        DupeFound := True;
        Break;
      end;
    l_DuplicateShortcuts.Visible := DupeFound;
  finally
    FreeAndNil(sl);
  end;
end;

function TfmEditorPopupMenuExpertConfig.GetExpertIndex(
  const _ListView: TListView;
  const _Expert: TGX_BaseExpert): Integer;
var
  i: Integer;
begin
  Result := -1;
  if not Assigned(_ListView) then
    Exit;
  if not Assigned(_Expert) then
    Exit;

  for i := 0 to _ListView.Items.Count - 1 do begin
    if _ListView.Items[i].Data = _Expert then begin
      Result := i;
      Break;
    end;
  end;
end;

procedure TfmEditorPopupMenuExpertConfig.AddExpert(const _Key: string; const _ExpName: string;
  _Expert: TGX_BaseExpert);
var
  li: TListItem;
begin
  if GetExpertIndex(lv_Selected, _Expert) >= 0 then begin
    Exit; // expert is already in "lv_Selected"
  end;

  li := lv_Selected.Items.Add;
  if _Key <> '' then
    li.Caption := _Key
  else
    li.Caption := LeftStr(_ExpName, 1);
  li.Data := _Expert;
  li.SubItems.Add(_Expert.GetDisplayName);
  CheckForDuplicates;
end;

procedure TfmEditorPopupMenuExpertConfig.GetData(_sl: TStringList; out _ForceEdiorActive: Boolean);
var
  i: Integer;
  li: TListItem;
begin
  _ForceEdiorActive := chk_FocusEditor.Checked;
  _sl.Clear;
  for i := 0 to lv_Selected.Items.Count - 1 do begin
    li := lv_Selected.Items[i];
    if Assigned(li.Data) then begin
      if TObject(li.Data) is TGX_BaseExpert then
        _sl.Add(li.Caption + '=' + TGX_BaseExpert(li.Data).GetName);
    end;
  end;
end;

procedure TfmEditorPopupMenuExpertConfig.lb_EditorExpertsDblClick(Sender: TObject);
begin
  AddSelectedEditorExpert;
end;

procedure TfmEditorPopupMenuExpertConfig.lb_ExpertsDblClick(Sender: TObject);
begin
  AddSelectedExpert;
end;

procedure TfmEditorPopupMenuExpertConfig.lv_SelectedChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  case Change of
    ctState, ctText:
      EnableOKCancel;
  end;
  CheckForDuplicates;
end;

procedure TfmEditorPopupMenuExpertConfig.lv_SelectedDblClick(Sender: TObject);
begin
  RemoveExpert;
end;

procedure TfmEditorPopupMenuExpertConfig.lv_SelectedEdited(Sender: TObject; Item: TListItem;
  var s: string);
begin
  EnableOKCancel;
  CheckForDuplicates;
end;

procedure TfmEditorPopupMenuExpertConfig.lv_SelectedEditing(Sender: TObject; Item: TListItem;
  var AllowEdit: Boolean);
begin
  b_Ok.Enabled := False;
  b_Cancel.Enabled := False;
end;

procedure TfmEditorPopupMenuExpertConfig.mi_EditHotkey(_Sender: TObject);
var
  li: TListItem;
begin
  if TListView_TryGetSelected(lv_Selected, li) then
    li.EditCaption;
end;

procedure TfmEditorPopupMenuExpertConfig.pm_SelectedPopup(Sender: TObject);
var
  li: TListItem;
  i: Integer;
  UsedHotkeys: set of AnsiChar;
  c: AnsiChar;
  mi: TMenuItem;
begin
  if not TListView_TryGetSelected(lv_Selected, li) then
    Exit; //==>

  pm_Selected.Items.Clear;

  Include(UsedHotkeys, 'X');

  for i := 0 to lv_Selected.Items.Count - 1 do begin
    c := AnsiChar(lv_Selected.Items[i].Caption[1]);
    Include(UsedHotkeys, c);
  end;

  for c := 'A' to 'Z' do begin
    if not (c in UsedHotkeys) then
      TPopupMenu_AppendMenuItem(pm_Selected, string(c), mi_SetHotkey);
  end;

  mi := TPopupMenu_AppendMenuItem(pm_Selected, 'Edit Hotkey', mi_EditHotkey);
  mi.ShortCut := ShortCut(VK_F2, []);
end;

procedure TfmEditorPopupMenuExpertConfig.mi_SetHotkey(_Sender: TObject);
var
  mi: TMenuItem;
  li: TListItem;
begin
  if not TListView_TryGetSelected(lv_Selected, li) then
    Exit; //==>
  mi := _Sender as TMenuItem;
  li.Caption := mi.Caption;
  CheckForDuplicates;
end;

procedure TfmEditorPopupMenuExpertConfig.RemoveExpert;
begin
  if lv_Selected.ItemIndex = -1 then
    Exit;
  lv_Selected.DeleteSelected;
  CheckForDuplicates;
end;

procedure TfmEditorPopupMenuExpertConfig.SetData(_sl: TStringList; _ForceEdiorActive: Boolean);
var
  i: Integer;
  Key: string;
  ExpName: string;
  idx: Integer;
begin
  chk_FocusEditor.Checked := _ForceEdiorActive;

  lv_Selected.OnChange := nil;
  lv_Selected.Items.BeginUpdate;
  try
    lv_Selected.Clear;
    for i := 0 to _sl.Count - 1 do begin
      Key := _sl.Names[i];
      ExpName := _sl.Values[Key];
      if GExpertsInst.FindExpert(ExpName, idx) then begin
        AddExpert(Key, ExpName, GExpertsInst.ExpertList[idx]);
      end else if GExpertsInst.EditorExpertManager.FindExpert(ExpName, idx) then begin
        AddExpert(Key, ExpName, GExpertsInst.EditorExpertManager.EditorExpertList[idx]);
      end;
    end;
  finally
    lv_Selected.Items.EndUpdate;
    lv_Selected.OnChange := lv_SelectedChange;
  end;
end;

initialization
  RegisterEditorExpert(TGxEditorPopupMenuExpert);
end.
