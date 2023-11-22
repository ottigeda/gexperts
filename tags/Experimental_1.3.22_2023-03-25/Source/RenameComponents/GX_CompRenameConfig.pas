unit GX_CompRenameConfig;

{$I GX_CondDefine.inc}

interface

uses
  Windows, SysUtils, Classes, Types, Forms, Controls, StdCtrls, Messages, Grids, Menus, Dialogs,
  ActnList, ExtCtrls, Graphics, Mask, Actions, ComCtrls,
  GX_SharedImages, GX_BaseForm;

const
  COMP_RENAME_NAME = 'RenameComponents';

const
  UM_SHOW_CONTROL = WM_USER + 133;

resourcestring
  SNotFound = 'not found: %s';

type
  TEditMode = (emRead, emEdit, emInsert);

  TOnExportLists = procedure(_Sender: TObject; _RulesListVcl, _RulesListFmx: TStringList) of object;
  TOnImportLists = procedure(_Sender: TObject; const _fn: string; _RulesListVcl, _RulesListFmx: TStringList) of object;

  TOnRowHeaderClick = procedure(Sender: TObject; Col: Integer) of object;

  TRenameStringGrid = class(TStringGrid)
  private
    FComponents: TStringList;
    FOnRowHeaderClick: TOnRowHeaderClick;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
  protected
    function GetEditStyle(ACol: Integer; ARow: Integer): TEditStyle; override;
    function CreateEditor: TInplaceEdit; override;
    procedure OnGetComponentList(ACol, ARow: Integer; Items: TStrings);
    procedure WMLButtonUp(var Msg: TWMLButtonUp); message WM_LBUTTONUP;
    procedure doRowHeaderClick(Col: Integer);
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ResizeColumns;
  published
    property OnRowHeaderClick: TOnRowHeaderClick read FOnRowHeaderClick write FOnRowHeaderClick;
  end;

  TfmCompRenameConfig = class(TfmBaseForm)
    chkShowDialog: TCheckBox;
    chkAutoAdd: TCheckBox;
    grpNames: TGroupBox;
    pnlRules: TPanel;
    pnlRight: TPanel;
    pnlIncSearch: TPanel;
    pnlNames: TPanel;
    edtFind: TEdit;
    btnClear: TButton;
    l_Find: TLabel;
    btnAdd: TButton;
    btnDelete: TButton;
    TheActionList: TActionList;
    acAdd: TAction;
    acDelete: TAction;
    acFind: TAction;
    acCancel: TAction;
    pmGrid: TPopupMenu;
    mnuAdd: TMenuItem;
    mnuDelete: TMenuItem;
    mnuSep1: TMenuItem;
    mnuFind: TMenuItem;
    acOK: TAction;
    mnuSep2: TMenuItem;
    acSortByClass: TAction;
    acSortByRule: TAction;
    mnuSort: TMenuItem;
    mnuSortByClass: TMenuItem;
    mnuSortByRule: TMenuItem;
    FindDialog: TFindDialog;
    pnlFooter: TPanel;
    pnlTop: TPanel;
    btnOtherProperties: TButton;
    acOtherProperties: TAction;
    mnuOtherProperties: TMenuItem;
    pnlButtonsRight: TPanel;
    btnOK: TButton;
    btnClose: TButton;
    btnHelp: TButton;
    b_Tools: TButton;
    pm_Extra: TPopupMenu;
    mi_ResetTo: TMenuItem;
    mi_ResetToDefault: TMenuItem;
    mi_Import: TMenuItem;
    mi_Export: TMenuItem;
    lbxOtherProps: TListBox;
    pc_Names: TPageControl;
    ts_NamesVcl: TTabSheet;
    ts_NamesFmx: TTabSheet;
    procedure acOtherPropertiesExecute(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure TheActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure acAddExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acFindExecute(Sender: TObject);
    procedure acCancelExecute(Sender: TObject);
    procedure acOKExecute(Sender: TObject);
    procedure acSortByClassExecute(Sender: TObject);
    procedure acSortByRuleExecute(Sender: TObject);
    procedure GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure GridMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FindDialogFind(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure edtFindChange(Sender: TObject);
    procedure edtFindKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure b_ToolsClick(Sender: TObject);
    procedure mi_ImportClick(Sender: TObject);
    procedure mi_ExportClick(Sender: TObject);
    procedure pc_NamesChange(Sender: TObject);
    procedure mi_ResetToDefaultClick(Sender: TObject);
  private
    FOnImport: TOnImportLists;
    FOnExport: TOnExportLists;
    FValueListVcl: TStringList;
    FValueListFmx: TStringList;
    FGridVcl: TRenameStringGrid;
    FGridFmx: TRenameStringGrid;
    class procedure GetDefaultsList(_Defaults: TStrings);
    function GetActiveGrid: TRenameStringGrid;
    function GetActiveValueList: TStringList;
    procedure SetActiveValueList(_ValueList: TStringList);
    procedure CopyValuesToGrid(_Values: TStringList; _Grid: TRenameStringGrid);
    procedure CopyGridToValues(_Grid: TRenameStringGrid; var _Values: TStringList);
    function IsEmptyRow(aGrid: TStringGrid; ARow: Integer): Boolean;
    function IsValidRule(ARow: Integer): Boolean;
    procedure GridDeleteRow(ARow: Integer);
    procedure GridAddRow;
    function RemoveEmptyBottomRow: Boolean;
    function RowHasComponent(aGrid: TStringGrid; ARow: Integer): Boolean;
    procedure HandleOnRowHeaderClick(Sender: TObject; Col: Integer);
    procedure SortByClass;
    procedure SortByRule;
    procedure GetData(_ValueListVcl, _ValueListFmx: TStringList; out _AutoShow, _AutoAdd: Boolean;
      out _FormWidth, _FormHeight: Integer);
    procedure SetData(_ValueListVcl, _ValueListFmx: TStringList; _AutoShow, _AutoAdd: Boolean;
      _FormWidth, _FormHeight: Integer; const _Selected: string);
    procedure SetLists(_ValueListVcl, _ValueListFmx: TStringList);
    procedure HandleOnGridClick(_Sender: TObject);
    procedure UpdateOtherProps;
    procedure UpdateGridEvents;
    procedure ResizeGrids;
    procedure SetDefault(_Which: string);
    procedure Import(const _fn: string);
  protected
{$IFDEF GX_IDE_IS_HIDPI_AWARE}
    procedure ApplyDpi(_NewDpi: Integer; _NewBounds: PRect); override;
{$ENDIF}
  public
    constructor Create(_Owner: TComponent); override;
    destructor Destroy; override;
    class function Execute(_Owner: TComponent; _OnImport: TOnImportLists; _OnExport: TOnExportLists;
      _ValueListVcl, _ValueListFmx: TStringList; var _AutoShow: Boolean;
      var _AutoAdd: Boolean; var _FormWidth, _FormHeight: Integer; const _Selected: string): Boolean;
  end;

implementation

{$R *.dfm}

uses
  Math, StrUtils,
  u_dzClassUtils, u_dzVclUtils, u_dzStringUtils, u_dzMiscUtils,
  GX_GenericUtils, GX_OtaUtils, GX_GxUtils, GX_CompRenameAdvanced,
  GX_MessageBox, GX_GExperts;

function CompareClassFunc(List: TStringList; Index1, Index2: Integer): Integer;
var
  S1, S2: string;
begin
  Assert(Assigned(List));
  S1 := List.Names[Index1];
  S2 := List.Names[Index2];
  Result := AnsiCompareStr(S1, S2);
end;

function CompareRuleFunc(List: TStringList; Index1, Index2: Integer): Integer;
var
  S1, S2: string;
begin
  Assert(Assigned(List));
  S1 := List.Values[List.Names[Index1]];
  S2 := List.Values[List.Names[Index2]];
  Result := AnsiCompareStr(S1, S2);
end;

{ TfmCompRenameConfig }

class function TfmCompRenameConfig.Execute(_Owner: TComponent; _OnImport: TOnImportLists; _OnExport: TOnExportLists;
  _ValueListVcl, _ValueListFmx: TStringList; var _AutoShow: Boolean;
  var _AutoAdd: Boolean; var _FormWidth, _FormHeight: Integer; const _Selected: string): Boolean;
var
  frm: TfmCompRenameConfig;
begin
  frm := TfmCompRenameConfig.Create(_Owner);
  try
    frm.FOnImport := _OnImport;
    frm.FOnExport := _OnExport;
    frm.SetData(_ValueListVcl, _ValueListFmx, _AutoShow, _AutoAdd, _FormWidth, _FormHeight, _Selected);

    Result := (frm.ShowModal = mrOK);
    if Result then
      frm.GetData(_ValueListVcl, _ValueListFmx, _AutoShow, _AutoAdd, _FormWidth, _FormHeight);
  finally
    FreeAndNil(frm);
  end;
end;

procedure CopyValueList(_Src, _Dest: TStringList);
var
  i: Integer;
  Additional: TStringList;
  sl: TStringList;
begin
  Assert(Assigned(_Src));
  Assert(Assigned(_Dest));

  TStrings_FreeAllObjects(_Dest).Clear;

  for i := 0 to _Src.Count - 1 do begin
    Additional := _Src.Objects[i] as TStringList;
    if Assigned(Additional) then begin
      sl := TStringList.Create;
      sl.Assign(Additional);
    end else
      sl := nil;
    _Dest.AddObject(_Src[i], sl);
  end;
end;

procedure TfmCompRenameConfig.SetLists(_ValueListVcl, _ValueListFmx: TStringList);
begin
  CopyValueList(_ValueListVcl, FValueListVcl);
  CopyValueList(_ValueListFmx, FValueListFmx);
  CopyValuesToGrid(FValueListVcl, FGridVcl);
  FGridVcl.Row := FGridVcl.FixedRows; // Go to the top of the grid
  CopyValuesToGrid(FValueListFmx, FGridFmx);
  FGridFmx.Row := FGridFmx.FixedRows; // Go to the top of the grid

  if GxOtaActiveDesignerIsFMX then begin
    pc_Names.ActivePage := ts_NamesFmx
  end else begin
    pc_Names.ActivePage := ts_NamesVcl;
  end;
  UpdateGridEvents;
  edtFind.Text := '';
  UpdateOtherProps;
end;

procedure TfmCompRenameConfig.SetData(_ValueListVcl, _ValueListFmx: TStringList;
  _AutoShow, _AutoAdd: Boolean; _FormWidth, _FormHeight: Integer; const _Selected: string);
begin
  Width := _FormWidth;
  Height := _FormHeight;
  chkShowDialog.Checked := _AutoShow;
  chkAutoAdd.Checked := _AutoAdd;

  SetLists(_ValueListVcl, _ValueListFmx);

  edtFind.Text := _Selected;
end;

procedure TfmCompRenameConfig.UpdateGridEvents;
var
  Grid: TRenameStringGrid;
begin
  FGridVcl.OnRowHeaderClick := nil;
  FGridVcl.OnClick := nil;
  FGridFmx.OnRowHeaderClick := nil;
  FGridFmx.OnClick := nil;
  Grid := GetActiveGrid;
  Grid.OnRowHeaderClick := HandleOnRowHeaderClick;
  Grid.OnClick := HandleOnGridClick;
end;

function TfmCompRenameConfig.GetActiveGrid: TRenameStringGrid;
begin
  if pc_Names.ActivePage = ts_NamesVcl then
    Result := FGridVcl
  else
    Result := FGridFmx;
end;

function TfmCompRenameConfig.GetActiveValueList: TStringList;
begin
  if pc_Names.ActivePage = ts_NamesVcl then
    Result := FValueListVcl
  else
    Result := FValueListFmx;
end;

procedure TfmCompRenameConfig.SetActiveValueList(_ValueList: TStringList);
begin
  if pc_Names.ActivePage = ts_NamesVcl then
    FValueListVcl := _ValueList
  else
    FValueListFmx := _ValueList;
end;

procedure TfmCompRenameConfig.GetData(_ValueListVcl, _ValueListFmx: TStringList;
  out _AutoShow: Boolean; out _AutoAdd: Boolean; out _FormWidth, _FormHeight: Integer);
begin
  CopyGridToValues(FGridVcl, FValueListVcl);
  CopyGridToValues(FGridFmx, FValueListFmx);
  _FormWidth := Width;
  _FormHeight := Height;
  _AutoShow := chkShowDialog.Checked;
  _AutoAdd := chkAutoAdd.Checked;
  CopyValueList(FValueListVcl, _ValueListVcl);
  CopyValueList(FValueListFmx, _ValueListFmx);
end;

function GetModulePath: string;
begin
  Result := ExtractFilePath(GetModuleName(HInstance));
end;

class procedure TfmCompRenameConfig.GetDefaultsList(_Defaults: TStrings);
var
  Path: string;
  sr: TSearchRec;
  s: string;
begin
  Path := GetModulePath;
  if 0 = FindFirst(Path + COMP_RENAME_NAME + '-*.ini', faAnyFile, sr) then begin
    try
      repeat
        s := ChangeFileExt(sr.Name, '');
        Delete(s, 1, Length(COMP_RENAME_NAME) + 1);
        _Defaults.Add(s);
      until 0 <> FindNext(sr);
    finally
      FindClose(sr);
    end;
  end;
end;

constructor TfmCompRenameConfig.Create(_Owner: TComponent);
const
  GridPad = 8;
resourcestring
  ClassCaption = 'Class';
  RenameRuleCaption = 'Rename Rule';

  function CreateGrid(_Parent: TWinControl; const _Name: string): TRenameStringGrid;
  begin
    Result := TRenameStringGrid.Create(Self);
    Result.Name := _Name;
    Result.Parent := _Parent;
{$IFDEF GX_HAS_ALIGN_WITH_MARGINS}
    Result.AlignWithMargins := True;
    Result.Margins.Top := GridPad;
    Result.Margins.Left := GridPad;
    Result.Margins.Right := GridPad;
    Result.Margins.Bottom := GridPad;
    result.Align := alClient;
{$ELSE}
    Result.SetBounds(GridPad, GridPad, _Parent.Width - (GridPad * 2), _Parent.Height - (GridPad * 2));
    Result.Anchors := [akLeft, akTop, akRight, akBottom];
{$ENDIF}
    Result.ColCount := 2;
    Result.DefaultColWidth := 150;
    Result.DefaultRowHeight := 17;
    Result.FixedCols := 0;
    Result.RowCount := 2;
    Result.Options := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing, goTabs, goAlwaysShowEditor, goThumbTracking];
    Result.PopupMenu := pmGrid;
    Result.ScrollBars := ssVertical;
    Result.TabOrder := 0;
    Result.Cells[0, 0] := ClassCaption;
    Result.Cells[1, 0] := RenameRuleCaption;
  end;
var
  st: TStringList;
  i: Integer;
  mi: TMenuItem;

begin
  inherited;

  TControl_SetMinConstraints(Self);

  TPanel_BevelNone([pnlRules, pnlRight, pnlIncSearch, pnlNames]);

  FGridVcl := CreateGrid(ts_NamesVcl, 'GridVcl');
  FGridFmx := CreateGrid(ts_NamesFmx, 'GridFmx');

  l_Find.Left := GridPad;
  btnClear.Left := pnlIncSearch.Width - btnClear.Width - GridPad;
  edtFind.Left := GridPad + l_Find.Width + 8;
  edtFind.Width := btnClear.Left - edtFind.Left;

  FValueListVcl := TStringList.Create;
  FValueListFmx := TStringList.Create;
  UpdateGridEvents;
  ResizeGrids;

  FreeAndNil(mi_ResetToDefault);
  st := TStringList.Create;
  try
    GetDefaultsList(st);
    for i := 0 to st.Count - 1 do begin
      mi := TMenuItem.Create(Self);
      mi.Caption := st[i];
      mi.OnClick := mi_ResetToDefaultClick;
      mi_ResetTo.Add(mi);
    end;
  finally
    st.Free;
  end;

  InitDpiScaler;
end;

destructor TfmCompRenameConfig.Destroy;
begin
  TStrings_FreeWithObjects(FValueListVcl);
  TStrings_FreeWithObjects(FValueListFmx);

  inherited;
end;

{$IFDEF GX_IDE_IS_HIDPI_AWARE}
procedure TfmCompRenameConfig.ApplyDpi(_NewDpi: Integer; _NewBounds: PRect);
var
  il: TImageList;
begin
  inherited;

  il := GExpertsInst.GetScaledSharedImages(_NewDpi);
  TheActionList.Images := il;
  pmGrid.Images := il;
end;
{$ENDIF}

procedure TfmCompRenameConfig.UpdateOtherProps;
var
  CompType: WideString;
  Index: Integer;
  Additional: TStringList;
  Grid: TRenameStringGrid;
  ValueList: TStringList;
begin
  Grid := GetActiveGrid;
  ValueList := GetActiveValueList;
  CompType := Trim(Grid.Cells[0, Grid.Row]);

  Index := ValueList.IndexOfName(CompType);
  if Index = -1 then begin
    lbxOtherProps.Items.Clear;
  end else begin
    Additional := ValueList.Objects[Index] as TStringList;
    if Assigned(Additional) then
      lbxOtherProps.Items.Assign(Additional)
    else
      lbxOtherProps.Items.Clear;
  end;
end;

procedure TfmCompRenameConfig.HandleOnGridClick(_Sender: TObject);
begin
  UpdateOtherProps;
end;

procedure TfmCompRenameConfig.HandleOnRowHeaderClick(Sender: TObject; Col: Integer);
begin
  if Col = 0 then
    SortByClass
  else
    SortByRule;
end;

procedure TfmCompRenameConfig.FormResize(Sender: TObject);
begin
  ResizeGrids;
end;

procedure TfmCompRenameConfig.ResizeGrids;
begin
  FGridVcl.ResizeColumns;
  FGridFmx.ResizeColumns;
end;

procedure TfmCompRenameConfig.CopyValuesToGrid(_Values: TStringList; _Grid: TRenameStringGrid);
var
  i: Integer;
  YOffset: Integer;
  SavedRow: Integer;
  s: string;
begin
  SavedRow := _Grid.Row;
  YOffset := _Grid.FixedRows;
  if Assigned(_Values) then
  begin
    // Having only one row makes the fixed row paint wrong
    _Grid.RowCount := Max(YOffset + _Values.Count, 2);
    for i := 0 to _Values.Count - 1 do
    begin
      s := _Values.Names[i];
      _Grid.Cells[0, YOffset + i] := s;
      _Grid.Cells[1, YOffset + i] := _Values.Values[s];
    end;
    if SavedRow < _Grid.RowCount then
      _Grid.Row := SavedRow;
  end;
  FormResize(Self);
end;

procedure TfmCompRenameConfig.CopyGridToValues(_Grid: TRenameStringGrid; var _Values: TStringList);
var
  i: Integer;
  OrgValuelist: TStringList;
  Index: Integer;
  CompType: WideString;
  Rule: string;
begin
  if Assigned(_Values) then
  begin
    // We need to keep the objects that are assigned to Values, so this is a bit more complicated
    OrgValueList := _Values;
    OrgValuelist.Sorted := True;
    // in particular, we create a new string list, so the Values parameter must be a var parameter
    _Values := TStringList.Create;
    for i := _Grid.FixedRows to _Grid.RowCount - 1 do begin
      CompType := Trim(_Grid.Cells[0, i]);
      Rule := Trim(_Grid.Cells[1, i]);
      if CompType <> '' then begin
        _Values.Add(CompType + '=' + Rule);
      end;
    end;
    // Now that we have copied the rules, we also need to copy the addtional properties
    // from the Objects.
    for i := 0 to _Values.Count - 1 do begin
      CompType := _Values.Names[i];
      Index := OrgValuelist.IndexOfName(CompType);
      if Index <> -1 then
      begin
        _Values.Objects[i] := OrgValuelist.Objects[Index];
        // Assign NIL to the original so it won't be freed
        OrgValuelist.Objects[Index] := nil;
      end;
    end;
    TStrings_FreeWithObjects(OrgValuelist);
  end;
end;

procedure TfmCompRenameConfig.edtFindChange(Sender: TObject);

  procedure FilterGrid(_Grid: TRenameStringGrid; const _Filter: string);
  var
    sl: TStringList;
    i: Integer;
  begin
    sl := TStringList.Create;
    try
      TStringGrid_GetCol(_Grid, 0, sl);
      for i := 0 to sl.Count - 1 do begin
        if StartsText(_Filter, sl[i]) then begin
          _Grid.Row := _Grid.FixedRows + i;
          Exit; //==>
        end;
      end;
    finally
      FreeAndNil(sl);
    end;
  end;

var
  s: string;
begin
  s := edtFind.Text;
  if s <> '' then begin
    FilterGrid(FGridVcl, s);
    FilterGrid(FGridFmx, s);
  end;
end;

procedure TfmCompRenameConfig.edtFindKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Grid: TRenameStringGrid;
begin
  Grid := GetActiveGrid;
  case Key of
    VK_DOWN: begin
        Grid.SetFocus;
        Key := 0;
      end;
    VK_UP, VK_NEXT, VK_PRIOR: begin
        Grid.Perform(WM_KEYDOWN, Key, 0);
        Grid.SetFocus;
        Key := 0;
      end;
  end;
end;

procedure TfmCompRenameConfig.TheActionListUpdate(Action: TBasicAction; var Handled: Boolean);
var
  Grid: TRenameStringGrid;
begin
  Grid := GetActiveGrid;
  acAdd.Enabled := IsValidRule(Grid.Row) and not IsEmptyRow(Grid, Grid.RowCount - 1);
  acDelete.Enabled := ((Grid.Row >= Grid.FixedRows) and (Grid.Row < Grid.RowCount));
  acOtherProperties.Enabled := RowHasComponent(Grid, Grid.Row);
end;

procedure TfmCompRenameConfig.acAddExecute(Sender: TObject);
var
  Grid: TRenameStringGrid;
begin
  Grid := GetActiveGrid;
  TryFocusControl(Grid);
  GridAddRow;
  FormResize(Self);
end;

procedure TfmCompRenameConfig.acDeleteExecute(Sender: TObject);
var
  Grid: TRenameStringGrid;
begin
  Grid := GetActiveGrid;
  TryFocusControl(Grid);
  GridDeleteRow(Grid.Row);
  if Grid.RowCount <= Grid.FixedRows then
    GridAddRow;
  FormResize(Self);
end;

procedure TfmCompRenameConfig.acFindExecute(Sender: TObject);
var
  P: TPoint;
begin
  // Move the find dialog below the grid
  P := Point(pnlNames.Left, pnlNames.Top + pnlNames.Height);
  FindDialog.Position := pnlNames.ClientToScreen(P);
  FindDialog.Execute;
end;

procedure TfmCompRenameConfig.acCancelExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfmCompRenameConfig.acOKExecute(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfmCompRenameConfig.SortByClass;
var
  Grid: TRenameStringGrid;
  ValueList: TStringList;
begin
  Grid := GetActiveGrid;
  ValueList := GetActiveValueList;
  CopyGridToValues(Grid, ValueList);
  // we just created a new ValueList and freed the old one so we need to assign it to the field
  SetActiveValueList(ValueList);
  ValueList.CustomSort(CompareClassFunc);
  CopyValuesToGrid(ValueList, Grid);
end;

procedure TfmCompRenameConfig.acSortByClassExecute(Sender: TObject);
begin
  SortByClass;
end;

procedure TfmCompRenameConfig.SortByRule;
var
  Grid: TRenameStringGrid;
  ValueList: TStringList;
begin
  Grid := GetActiveGrid;
  ValueList := GetActiveValueList;
  CopyGridToValues(Grid, ValueList);
  // we just created a new ValueList and freed the old one so we need to assign it to the field
  SetActiveValueList(ValueList);
  ValueList.CustomSort(CompareRuleFunc);
  CopyValuesToGrid(ValueList, Grid);
end;

procedure TfmCompRenameConfig.acSortByRuleExecute(Sender: TObject);
begin
  SortByRule;
end;

procedure TfmCompRenameConfig.GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Grid: TRenameStringGrid;
begin
  Grid := GetActiveGrid;
  if Shift = [] then
  begin
    case Key of
      VK_DOWN:
        begin
          // If we are at bottom of grid, add a new empty row
          if (Grid.Row = Grid.RowCount - 1) and not IsEmptyRow(Grid, Grid.Row) then
            Grid.RowCount := Grid.RowCount + 1;
        end;

      VK_UP:
        begin
          // If we are at the bottom of grid, and this is an empty row, remove it
          if RemoveEmptyBottomRow then
            Key := 0;
        end;
    end; { case }
  end; { if Shift }
end; { GridKeyDown }

function TfmCompRenameConfig.IsEmptyRow(aGrid: TStringGrid; ARow: Integer): Boolean;
begin
  Result := Trim(aGrid.Rows[ARow].Text) = '';
end;

function TfmCompRenameConfig.RowHasComponent(aGrid: TStringGrid; ARow: Integer): Boolean;
var
  Grid: TRenameStringGrid;
begin
  Grid := GetActiveGrid;
  Result := (Trim(Grid.Cells[0, ARow]) > '');
end;

function TfmCompRenameConfig.IsValidRule(ARow: Integer): Boolean;
var
  Grid: TRenameStringGrid;
begin
  Grid := GetActiveGrid;
  Result := (Trim(Grid.Cells[0, ARow]) > '') and
            (Trim(Grid.Cells[1, ARow]) > '');
end;

procedure TfmCompRenameConfig.pc_NamesChange(Sender: TObject);
begin
  UpdateGridEvents;
  UpdateOtherProps;
end;

function TfmCompRenameConfig.RemoveEmptyBottomRow: Boolean;
var
  Grid: TRenameStringGrid;
begin
  Grid := GetActiveGrid;
  if IsEmptyRow(Grid, Grid.RowCount - 1) and (Grid.RowCount > Grid.FixedRows) then
  begin
    Grid.RowCount := Grid.RowCount - 1;
    Result := True;
  end
  else
    Result := False;
end;

procedure TfmCompRenameConfig.GridAddRow;
var
  Grid: TRenameStringGrid;
begin
  Grid := GetActiveGrid;
  Grid.RowCount := Grid.RowCount + 1;
  Application.ProcessMessages;
  Grid.Col := Grid.FixedCols;
  Grid.Row := Grid.RowCount - 1;
end;

procedure TfmCompRenameConfig.GridDeleteRow(ARow: Integer);
var
  i, j: Integer;
  Grid: TRenameStringGrid;
begin
  Grid := GetActiveGrid;
  if ARow > Grid.FixedRows - 1 then
  begin
    if ARow < Grid.RowCount - 1 then
    begin
      // Move all cells one row up
      for i := ARow to Grid.RowCount - 2 do
        for j := 0 to Grid.ColCount - 1 do
          Grid.Cells[j, i] := Grid.Cells[j, i + 1];
    end;
    // Delete the last row
    Grid.Rows[Grid.RowCount - 1].Clear;
    if Grid.RowCount > Grid.FixedRows + 1 then
      Grid.RowCount := Grid.RowCount - 1;
  end;
end;

procedure TfmCompRenameConfig.GridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  Grid: TRenameStringGrid;
begin
  Grid := GetActiveGrid;
  if (ARow < Grid.Row) and IsEmptyRow(Grid, Grid.Row) then
    RemoveEmptyBottomRow;
  if (ACol > 0) and (Grid.Cells[0, ARow]='') then
    CanSelect := False;
end;

procedure TfmCompRenameConfig.GridMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  CellCoord: TGridCoord;
  Grid: TRenameStringGrid;
begin
  Grid := GetActiveGrid;
  CellCoord := Grid.MouseCoord(X, Y);
  if CellCoord.Y = 0 then
  begin
    case CellCoord.X of
      0: acSortByClass.Execute;
      1: acSortByRule.Execute;
    end;
  end;
end;

procedure TfmCompRenameConfig.FindDialogFind(Sender: TObject);
var
  Index: Integer;
  FoundRow: Integer;
  FoundCol: Integer;
  FindMsg: string;
  Grid: TRenameStringGrid;
begin
  Grid := GetActiveGrid;
  Index := Grid.Row + 1;
  FoundRow := -1;
  FoundCol := 0;
  while (FoundRow < 0) and (Index < Grid.RowCount) do
  begin
    if CaseInsensitivePos(FindDialog.FindText, Grid.Cells[1, Index]) > 0 then
    begin
      FoundCol := 1;
      FoundRow := Index;
    end;

    if CaseInsensitivePos(FindDialog.FindText, Grid.Cells[0, Index]) > 0 then
    begin
      FoundCol := 0;
      FoundRow := Index;
    end;

    Inc(Index);
  end;
  if FoundRow >= 0 then
  begin
    Grid.Row := FoundRow;
    Grid.Col := FoundCol;
    Application.ProcessMessages;
    FindDialog.CloseDialog;
  end
  else begin
    FindMsg := Format(SNotFound, [FindDialog.FindText]);
    MessageDlg(FindMsg, mtInformation, [mbOK], 0);
  end;
end;

{ TDefaultRenameComponentsMessage }

type
  TDefaultRenameComponentsMessage = class(TGxQuestionBoxAdaptor)
  protected
    function GetMessage: string; override;
  end;

function TDefaultRenameComponentsMessage.GetMessage: string;
resourcestring
  SSetToDefaults =
    'This will reset this expert to the default settings. ' +
    'In particular it will replace existing rename rules with the defaults. ' +
    'Do you really want to do that?';
begin
  Result := SSetToDefaults;
end;


procedure TfmCompRenameConfig.btnClearClick(Sender: TObject);
begin
  edtFind.Text := '';
  edtFind.SetFocus;
end;

procedure TfmCompRenameConfig.btnHelpClick(Sender: TObject);
begin
  GxContextHelp(Self, 42);
end;

procedure TfmCompRenameConfig.mi_ExportClick(Sender: TObject);
begin
  FOnExport(Self, FValueListVcl,FValueListFmx);
end;


procedure TfmCompRenameConfig.mi_ImportClick(Sender: TObject);
var
  fn: string;
begin
  fn := 'GExperts_' + COMP_RENAME_NAME + '.ini';
  if not ShowOpenDialog('Select file to import', 'ini', fn, 'INI files (*.ini)|*.ini') then
    Exit; //==>
  Import(fn);
end;

procedure TfmCompRenameConfig.Import(const _fn: string);
var
  RulesListVcl: TStringList;
  RulesListFmx: TStringList;
begin
  if not FileExists(_fn) then
    Exit; //==>
  InitializeNil(RulesListVcl, RulesListFmx);
  try
    RulesListVcl := TStringList.Create;
    RulesListFmx := TStringList.Create;
    FOnImport(Self, _fn, RulesListVcl, RulesListFmx);
    SetLists(RulesListVcl, RulesListFmx);
  finally
    FreeAndNil(RulesListVcl, RulesListFmx);
  end;
end;

procedure TfmCompRenameConfig.SetDefault(_Which: string);
var
  Path: string;
begin
  if (FValueListVcl.Count > 0) or (FValueListFmx.Count > 0) then begin
    if ShowGxMessageBox(TDefaultRenameComponentsMessage) <> mrYes then
      Exit; //==>
  end;

  _Which := StringReplace(_Which, '&', '', [rfReplaceAll]);
  Path := GetModulePath;
  Import(Path + COMP_RENAME_NAME + '-' + _Which + '.ini');
end;

procedure TfmCompRenameConfig.mi_ResetToDefaultClick(Sender: TObject);
begin
  SetDefault(TMenuItem(Sender).Caption);
end;

procedure TfmCompRenameConfig.b_ToolsClick(Sender: TObject);
var
  Point: TPoint;
begin
  Point.X := b_Tools.Width;
  Point.Y := 0;
  Point := b_Tools.ClientToScreen(Point);
  pm_Extra.Popup(Point.X, Point.Y);
end;

procedure TfmCompRenameConfig.acOtherPropertiesExecute(Sender: TObject);
var
  CompType: WideString;
  Rule: WideString;
  Index: Integer;
  Additional: TStringList;
  Grid: TRenameStringGrid;
  ValueList: TStringList;
begin
  Grid := GetActiveGrid;
  ValueList := GetActiveValueList;
  CompType := Trim(Grid.Cells[0, Grid.Row]);
  Rule := Trim(Grid.Cells[1, Grid.Row]);
  Index := ValueList.IndexOfName(CompType);
  if Index = -1 then
    Index := ValueList.Add(CompType + '=' + Rule);
  if Index <> -1 then
    begin
      Additional := ValueList.Objects[Index] as TStringList;
      if TfmCompRenameAdvanced.Execute(Self, CompType, Additional) then
        ValueList.Objects[Index] := Additional;
    end;
  UpdateOtherProps;
  if Grid.CanFocus then
    ActiveControl := Grid;
end;

{ TRenameStringGrid }

const
  clEditFocused   = clHighlight;  // e.g. $D77800;
  clEditUnfocused = clBtnFace;    // e.g. $f0f0f0;

procedure TRenameStringGrid.CMEnter(var Message: TCMEnter);
begin
  inherited;
  if Assigned(InplaceEditor) then
    TMaskEdit(InplaceEditor).Color := clEditFocused;
end;

procedure TRenameStringGrid.CMExit(var Message: TCMExit);
begin
  if Assigned(InplaceEditor) then
    TMaskEdit(InplaceEditor).Color := clEditUnfocused;
end;

constructor TRenameStringGrid.Create(AOwner: TComponent);
begin
  inherited;
  FComponents := TStringList.Create;
  GxOtaGetInstalledComponentList(FComponents, False);
end;

destructor TRenameStringGrid.Destroy;
begin
  FreeAndNil(FComponents);
  inherited;
end;

procedure TRenameStringGrid.doRowHeaderClick(Col: Integer);
begin
  if Assigned(FOnRowHeaderClick) then
    FOnRowHeaderClick(Self, Col);
end;

procedure TRenameStringGrid.WMLButtonUp(var msg: TWMLButtonUp);
var
  ACol, ARow: Integer;
begin
  inherited;
  MouseToCell(Msg.XPos, Msg.YPos, ACol, ARow);
  if ARow <= FixedRows-1 then
    doRowHeaderClick(ACol);
end;

type
  TGxInplaceEditList = class(TInplaceEditList)
  public
    property Color;
  end;

function TRenameStringGrid.CreateEditor: TInplaceEdit;
var
  cmb: TGxInplaceEditList;
begin
  cmb := TGxInplaceEditList.Create(Self);
  cmb.OnGetPickListitems := OnGetComponentList;
  cmb.DropDownRows := 15;
  if Focused then
    cmb.Color := clEditFocused
  else
    cmb.Color := clEditUnfocused;
  Result := cmb;
end;

function TRenameStringGrid.GetEditStyle(ACol, ARow: Integer): TEditStyle;
begin
  if ACol = 0 then
    Result := esPickList
  else
    Result := esSimple;
end;

procedure TRenameStringGrid.OnGetComponentList(ACol, ARow: Integer; Items: TStrings);
begin
  Items.Assign(FComponents);
end;

procedure TRenameStringGrid.ResizeColumns;
begin
  ColWidths[0] := (ClientWidth div 2) - 1;
  ColWidths[1] := ColWidths[0];
end;

procedure TRenameStringGrid.Resize;
begin
  ResizeColumns;
  inherited;
end;

end.

