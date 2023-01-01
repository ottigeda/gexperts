unit GX_FormHotkeys;

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
  Grids,
  StdCtrls,
  ActnList,
  Actions,
  Menus,
  u_dzTypes,
  GX_BaseForm;

type
  PComponentRec = ^TComponentRec;
  TComponentRec = record
    CompName: string;
    CompType: string;
    Caption: string;
    Accelerator: string;
    Shortcut: string;
    SecondaryShortcuts: TStringArray;
    Action: string;
    HasShortcutProperty: Boolean;
    HasSecodaryShortcutProperty: Boolean;
  end;

type
  TComponentRecArr = array of TComponentRec;

type
  Tf_FormHotkeys = class(TfmBaseForm)
    TheGrid: TStringGrid;
    TheActionList: TActionList;
    act_AssignShortcut: TAction;
    pm_Grid: TPopupMenu;
    pm_AssingShortcut: TMenuItem;
    act_GotoAction: TAction;
    mi_GotoAction: TMenuItem;
    procedure TheGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
      State: TGridDrawState);
    procedure TheGridMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint;
      var Handled: Boolean);
    procedure TheGridMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint;
      var Handled: Boolean);
    procedure act_AssignShortcutExecute(Sender: TObject);
    procedure pm_GridPopup(Sender: TObject);
    procedure act_GotoActionExecute(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FCompRecs: TComponentRecArr;
    function ScrollGrid(_Grid: TStringGrid; _Direction: Integer; _Shift: TShiftState;
      _MousePos: TPoint): Boolean;
  protected
{$IFDEF IDE_IS_HIDPI_AWARE}
    FOldDPI: Integer;
    procedure ApplyDpi(_NewDpi: Integer; _NewBounds: PRect); override;
{$ENDIF}
  public
    constructor Create(_Owner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

uses
  ToolsAPI,
  TypInfo,
  Math,
  u_dzVclUtils,
  GX_OtaUtils,
  GX_Experts,
  GX_GetIdeVersion,
  GX_StringGridDrawFix,
  ComCtrls,
  GX_FormHotkeysSelect;

type
  TFormHotkeysExpert = class(TGX_Expert)
  protected
    procedure UpdateAction(aAction: TCustomAction); override;
  public
    function GetActionCaption: string; override;
    procedure Execute(Sender: TObject); override;
    function HasConfigOptions: Boolean; override;
  end;

const
  COL_NAME = 0;
  COL_TYPE = 1;
  COL_CAPTION = 2;
  COL_ACCEL = 3;
  COL_ACTION = 4;
  COL_SHORTCUT = 5;
  COL_SECONDARY = 6;

{ Tf_FormHotkeys }

constructor Tf_FormHotkeys.Create(_Owner: TComponent);
var
  UsedActions: TList;

  function IsActionUsed(_Action: TObject): Boolean;
  var
    i: Integer;
  begin
    Result := True;
    for i := 0 to UsedActions.Count - 1 do
      if UsedActions[i] = _Action then
        Exit; //==>
    Result := False;
  end;

var
  FormEditor: IOTAFormEditor;
  Root: IOTAComponent;
  cmp: IOTAComponent;
  i: Integer;
  Offset: Integer;
  cnt: Integer;
  rec: PComponentRec;
  s: string;
  Shortcut: TShortCut;
  Obj: TObject;
  sl: TStringList;
  Action: TObject;
  j: Integer;
  NativeCmp: TComponent;
  Info: PPropInfo;
  SecondaryShortcutCount: Integer;
  MaxSecondaryShortcutCount: Integer;
begin
  inherited;

  TheGrid.Cells[COL_NAME, 0] := 'Component';
  TheGrid.Cells[COL_TYPE, 0] := 'Type';
  TheGrid.Cells[COL_CAPTION, 0] := 'Caption';
  TheGrid.Cells[COL_ACCEL, 0] := 'Accelerator';
  TheGrid.Cells[COL_ACTION, 0] := 'Action';
  TheGrid.Cells[COL_SHORTCUT, 0] := 'Shortcut';
  TheGrid.Cells[COL_SECONDARY, 0] := 'Secondary Shortcuts';

  TGrid_SetNonfixedRowCount(TheGrid, 0);
  if not GxOtaTryGetCurrentFormEditor(FormEditor) then
    Exit; //==>

  Root := FormEditor.GetRootComponent;
  Root.GetComponentType;
  Offset := Length(FCompRecs);

  UsedActions := TList.Create;
  try
    MaxSecondaryShortcutCount := 1;
    cnt := Root.GetComponentCount;
    SetLength(FCompRecs, Offset + cnt);
    for i := 0 to cnt - 1 do begin
      cmp := Root.GetComponent(i);
      // I found no working way to access the SecondaryShortCuts value via IOTAComponent, so
      // I'm going through the native component
      NativeCmp := GxOtaGetNativeComponent(cmp);
      rec := @FCompRecs[Offset + i];
      rec.CompType := NativeCmp.ClassName;
      rec.CompName := NativeCmp.Name;

      // Is there an Action property?
      Info := TypInfo.GetPropInfo(NativeCmp.ClassInfo, 'Action');
      if not Assigned(Info) or (Info.PropType^.Kind <> tkClass) then begin
        Action := nil;
      end else begin
        // Is it assigned?
        Action := TypInfo.GetObjectProp(NativeCmp, Info);
        if Assigned(Action) and (Action is TBasicAction) then begin
          // and it is assigned
          // -> Ignore any hotkeys this control might have, but save the action as one that's
          // assigned to a control.
          UsedActions.Add(Action);
        end else
          Action := nil;
      end;

      if cmp.GetPropValueByName('Caption', s) then begin
        rec.Caption := s;

        if (NativeCmp is TBasicAction) and not IsActionUsed(NativeCmp) then begin
          // Actions cannot be called by their accelerator key unless they are assigned to a control
          // todo: Is that really true? I seem to remember it differently.
          rec.Accelerator := 'N/A';
        end else begin
          s := Menus.GetHotkey(s);
          if s <> '' then
            rec.Accelerator := 'Alt+' + UpperCase(s)
          else
            rec.Accelerator := '';
        end;
      end else begin
        rec.Caption := 'N/A';
        rec.Accelerator := 'N/A';
      end;

      if Assigned(Action) then begin
        rec.Shortcut := 'N/A';
        rec.Action := TBasicAction(Action).Name;
      end else begin
        rec.Action := '';
        if cmp.GetPropValueByName('ShortCut', Shortcut) then begin
          rec.Shortcut := Menus.ShortCutToText(Shortcut);
          rec.HasShortcutProperty := True;
        end else begin
          rec.Shortcut := 'N/A';
          rec.HasShortcutProperty := False;
        end;

        Info := TypInfo.GetPropInfo(NativeCmp.ClassInfo, 'SecondaryShortCuts');
        if Assigned(Info) and (Info.PropType^.Kind = tkClass) then begin
          rec.HasSecodaryShortcutProperty := True;
          Obj := TypInfo.GetObjectProp(NativeCmp, Info);
          if Assigned(Obj) and (Obj is TStringList) then begin
            sl := TStringList(Obj);
            SecondaryShortcutCount := sl.Count;
            SetLength(rec.SecondaryShortcuts, SecondaryShortcutCount);
            for j := 0 to SecondaryShortcutCount - 1 do
              rec.SecondaryShortcuts[j] := sl[j];
            if SecondaryShortcutCount > MaxSecondaryShortcutCount then
              MaxSecondaryShortcutCount := SecondaryShortcutCount;
          end;
        end else begin
          rec.HasSecodaryShortcutProperty := False;
        end;
      end;
    end;
  finally
    FreeAndNil(UsedActions);
  end;

  cnt := Length(FCompRecs);
  TGrid_SetNonfixedRowCount(TheGrid, cnt);
  Offset := TheGrid.FixedRows;
  if MaxSecondaryShortcutCount > 1 then
    TGrid_SetNonfixedColCount(TheGrid, MaxSecondaryShortcutCount + 5);
  for i := 0 to cnt - 1 do begin
    rec := @FCompRecs[i];
    TheGrid.Cells[COL_NAME, Offset + i] := rec.CompName;
    TheGrid.Cells[COL_TYPE, Offset + i] := rec.CompType;
    TheGrid.Cells[COL_CAPTION, Offset + i] := rec.Caption;
    TheGrid.Cells[COL_ACCEL, Offset + i] := rec.Accelerator;
    TheGrid.Cells[COL_ACTION, Offset + i] := rec.Action;
    TheGrid.Cells[COL_SHORTCUT, Offset + i] := rec.Shortcut;
    for j := 0 to MaxSecondaryShortcutCount - 1 do begin
      if not rec.HasSecodaryShortcutProperty then begin
        TheGrid.Cells[COL_SECONDARY + j, Offset + i] := 'N/A';
      end else begin
        if j < Length(rec.SecondaryShortcuts) then begin
          TheGrid.Cells[COL_SECONDARY + j, Offset + i] := rec.SecondaryShortcuts[j];
        end else begin
          TheGrid.Cells[COL_SECONDARY + j, Offset + i] := '';
        end;
      end;
    end;
  end;

  TGrid_Resize(TheGrid, [roUseGridWidth, roUseAllRows]);

  InitDpiScaler;
end;

destructor Tf_FormHotkeys.Destroy;
begin

  inherited;
end;

procedure Tf_FormHotkeys.FormResize(Sender: TObject);
begin
  TGrid_Resize(TheGrid, [roUseGridWidth, roUseAllRows]);
end;

{$IFDEF IDE_IS_HIDPI_AWARE}
procedure Tf_FormHotkeys.ApplyDpi(_NewDpi: Integer; _NewBounds: PRect);
begin
  if FOldDPI = 0 then
    FOldDPI := TForm_CurrentPPI(Self);
  if Assigned(FScaler) then
    FScaler.ApplyDpi(_NewDpi, _NewBounds);
  TStringGrid_AdjustRowHeight(TheGrid);
end;
{$ENDIF}

procedure Tf_FormHotkeys.pm_GridPopup(Sender: TObject);
var
  FormEditor: IOTAFormEditor;
  r: Integer;
  rec: PComponentRec;
  IsAssignShortcutAllowed: Boolean;
  IsGotoActionAllowed: Boolean;
begin
  IsAssignShortcutAllowed := False;
  IsGotoActionAllowed := False;
  if GxOtaTryGetCurrentFormEditor(FormEditor) then begin
    r := TGrid_GetNonfixedRow(TheGrid);
    if (r >= Low(FCompRecs)) and (r <= High(FCompRecs)) then begin
      rec := @FCompRecs[r];
      if (rec.Action = '') and rec.HasShortcutProperty then
        IsAssignShortcutAllowed := True;
      if rec.Action <> '' then
        IsGotoActionAllowed := True;
    end;
  end;
  act_AssignShortcut.Enabled := IsAssignShortcutAllowed;
  act_GotoAction.Enabled := IsGotoActionAllowed;
end;

procedure Tf_FormHotkeys.act_AssignShortcutExecute(Sender: TObject);
var
  FormEditor: IOTAFormEditor;
  Root: IOTAComponent;
  cnt: Integer;
  i: Integer;
  rec: PComponentRec;
  cmp: IOTAComponent;
  NativeCmp: TComponent;
  r: Integer;
  CompName: string;
  CompShortcut: TShortCut;
begin
  if not GxOtaTryGetCurrentFormEditor(FormEditor) then
    Exit; //==>

  r := TGrid_GetNonfixedRow(TheGrid);
  if (r < Low(FCompRecs)) or (r > High(FCompRecs)) then
    Exit; //==>

  rec := @FCompRecs[r];
  if (rec.Action <> '') or not rec.HasShortcutProperty then
    Exit; //==>

  CompName := rec.CompName;

  Root := FormEditor.GetRootComponent;
  cnt := Root.GetComponentCount;
  for i := 0 to cnt - 1 do begin
    cmp := Root.GetComponent(i);
    NativeCmp := GxOtaGetNativeComponent(cmp);
    if SameText(NativeCmp.Name, CompName) then begin
      if rec.Shortcut <> 'N/A' then
        CompShortcut := TextToShortCut(rec.Shortcut)
      else
        CompShortcut := 0;
      if TfmFormHotkeysSelect.Execute(Self, CompName, CompShortcut) then begin
        if cmp.SetPropByName('ShortCut', CompShortcut) then begin
          if cmp.GetPropValueByName('ShortCut', CompShortcut) then begin
            rec.Shortcut := ShortCutToText(CompShortcut);
            TStringGrid_SetNonfixedCell(TheGrid, COL_SHORTCUT, r, rec.Shortcut);
          end;
        end;
      end;
      Exit; //==>
    end;
  end;
end;

procedure Tf_FormHotkeys.act_GotoActionExecute(Sender: TObject);
var
  FormEditor: IOTAFormEditor;
  Root: IOTAComponent;
  cnt: Integer;
  i: Integer;
  rec: PComponentRec;
  cmp: IOTAComponent;
  NativeCmp: TComponent;
  r: Integer;
begin
  if not GxOtaTryGetCurrentFormEditor(FormEditor) then
    Exit; //==>

  r := TGrid_GetNonfixedRow(TheGrid);
  if (r < Low(FCompRecs)) or (r > High(FCompRecs)) then
    Exit; //==>

  rec := @FCompRecs[r];
  if rec.Action = '' then
    Exit; //==>

  Root := FormEditor.GetRootComponent;
  cnt := Root.GetComponentCount;
  for i := 0 to cnt - 1 do begin
    cmp := Root.GetComponent(i);
    NativeCmp := GxOtaGetNativeComponent(cmp);
    if SameText(NativeCmp.Name, rec.Action) then begin
      TGrid_SetNonfixedRow(TheGrid, i);
      Exit; //==>
    end;
  end;
end;

procedure Tf_FormHotkeys.TheGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
  State: TGridDrawState);
var
  sg: TStringGrid absolute Sender;
  cnv: TCanvas;
  s: string;
begin
  cnv := sg.Canvas;
  cnv.Font := sg.Font;
  if (gdSelected in State) and (not (gdFocused in State) or
    ([goDrawFocusSelected, goRowSelect] * sg.Options <> [])) then begin
    cnv.Brush.Color := clHighlight;
    cnv.Font.Color := clHighlightText;
  end else if gdFixed in State then begin
    cnv.Brush.Color := clBtnFace;
    cnv.Font.Color := clWindowText;
  end else begin
    cnv.Brush.Color := sg.Color;
    cnv.Font.Color := clWindowText;
  end;
  cnv.FillRect(Rect);

  s := sg.Cells[ACol, ARow];
  if s = 'N/A' then begin
    s := '';
    if not (gdSelected in State) then
      cnv.Brush.Color := clSilver;
  end;
  TStringGrid_DrawCellFixed(sg, s, Rect, State, sg.Focused);
end;

function Tf_FormHotkeys.ScrollGrid(_Grid: TStringGrid; _Direction: Integer;
  _Shift: TShiftState; _MousePos: TPoint): Boolean;
var
  LScrollLines: Integer;
  NewRow: Integer;
begin
  if SystemParametersInfo(SPI_GETWHEELSCROLLLINES, 0, @LScrollLines, 0) then begin
    if _Shift = [ssCtrl] then
      LScrollLines := LScrollLines * 3; // Ctrl = Speed
    if _Shift = [ssShift] then
      LScrollLines := 1;

    NewRow := _Grid.Row + (_Direction * LScrollLines);
    NewRow := Max(_Grid.FixedRows, NewRow); // Limit to top row
    NewRow := Min(_Grid.RowCount - 1, NewRow); // Limit to bottom row

    _Grid.Row := NewRow;
    Result := True;
  end else
    Result := False;
end;

procedure Tf_FormHotkeys.TheGridMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if (Sender is TStringGrid) then
    if ScrollGrid(TStringGrid(Sender), +1, Shift, MousePos) then
      Handled := True;
end;

procedure Tf_FormHotkeys.TheGridMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint;
  var Handled: Boolean);
begin
  if Sender is TStringGrid then
    if ScrollGrid(TStringGrid(Sender), -1, Shift, MousePos) then
      Handled := True;
end;

{ TFormHotkeysExpert }

procedure TFormHotkeysExpert.Execute(Sender: TObject);
var
  frm: Tf_FormHotkeys;
begin
  inherited;
  frm := Tf_FormHotkeys.Create(Application);
  frm.ShowModal;
end;

function TFormHotkeysExpert.GetActionCaption: string;
resourcestring
  SFormHotkeysExpertsCaption = 'Form Hotkeys';
begin
  Result := SFormHotkeysExpertsCaption;
end;

function TFormHotkeysExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

procedure TFormHotkeysExpert.UpdateAction(aAction: TCustomAction);
begin
  aAction.Enabled := GxOtaCurrentlyEditingForm;
end;

initialization
  RegisterGX_Expert(TFormHotkeysExpert);

finalization
//  FreeAndNil(TheForm);
end.

