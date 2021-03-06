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
  end;

type
  TComponentRecArr = array of TComponentRec;

type
  Tf_FormHotkeys = class(TfmBaseForm)
    l_HotAndAcceleratorKeys: TLabel;
    TheGrid: TStringGrid;
    TheActionList: TActionList;
    act_: TAction;
    pm_: TPopupMenu;
    SomeMenuItem1: TMenuItem;
    b_somebutton: TButton;
    procedure TheGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
      State: TGridDrawState);
    procedure TheGridMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint;
      var Handled: Boolean);
    procedure TheGridMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint;
      var Handled: Boolean);
  private
    FCompRecs: TComponentRecArr;
    function ScrollGrid(_Grid: TStringGrid; _Direction: Integer; _Shift: TShiftState;
      _MousePos: TPoint): Boolean;
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
  GX_StringGridDrawFix;

type
  TFormHotkeysExpert = class(TGX_Expert)
  protected
    procedure UpdateAction(aAction: TCustomAction); override;
  public
    function GetActionCaption: string; override;
    procedure Execute(Sender: TObject); override;
    function HasConfigOptions: Boolean; override;
  end;

{ Tf_FormHotkeys }

constructor Tf_FormHotkeys.Create(_Owner: TComponent);
var
  UsedActions: TList;

  function IsActionUsed(_Action: TObject): Boolean;
  var
    i: Integer;
  begin
    Result := True;
    for i := 0 to UsedActions.count - 1 do
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

  TheGrid.Cells[0, 0] := 'Component';
  TheGrid.Cells[1, 0] := 'Type';
  TheGrid.Cells[2, 0] := 'Caption';
  TheGrid.Cells[3, 0] := 'Accelerator';
  TheGrid.Cells[4, 0] := 'Shortcut';
  TheGrid.Cells[5, 0] := 'Secondary Shortcuts';

  TGrid_SetNonfixedRowCount(TheGrid, 0);
  if not GxOtaTryGetCurrentFormEditor(FormEditor) then
    Exit; //==>

  Root := FormEditor.GetRootComponent;
  Root.GetComponentType;
  Offset := Length(FCompRecs);

  UsedActions := TList.Create;
  try
    MaxSecondaryShortcutCount := 0;
    cnt := Root.GetComponentCount;
    SetLength(FCompRecs, Offset + cnt);
    for i := 0 to cnt - 1 do begin
      cmp := Root.GetComponent(i);
      // I found no working way to access the SecondaryShortCuts value via IOTAComponent, so
      // I'm going through the nataive component
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
          rec.Accelerator := 'N/A';
        end else begin
          s := Menus.GetHotkey(s);
          if s <> '' then
            rec.Accelerator := 'Alt+' + s
          else
            rec.Accelerator := '';
        end;
      end else begin
        rec.Caption := 'N/A';
        rec.Accelerator := 'N/A';
      end;

      if Assigned(Action) then begin
        rec.Shortcut := 'N/A';
      end else begin
        if cmp.GetPropValueByName('ShortCut', Shortcut) then begin
          rec.Shortcut := Menus.ShortCutToText(Shortcut);
        end else begin
          rec.Shortcut := 'N/A';
        end;

        Info := TypInfo.GetPropInfo(NativeCmp.ClassInfo, 'SecondaryShortCuts');
        if Assigned(Info) and (Info.PropType^.Kind = tkClass) then begin
          Obj := TypInfo.GetObjectProp(NativeCmp, Info);
          if Assigned(Obj) and (Obj is TStringList) then begin
            sl := TStringList(Obj);
            SecondaryShortcutCount := sl.count;
            SetLength(rec.SecondaryShortcuts, SecondaryShortcutCount);
            for j := 0 to SecondaryShortcutCount - 1 do
              rec.SecondaryShortcuts[j] := sl[j];
            if SecondaryShortcutCount > MaxSecondaryShortcutCount then
              MaxSecondaryShortcutCount := SecondaryShortcutCount;
          end;
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
    TheGrid.Cells[0, Offset + i] := rec.CompName;
    TheGrid.Cells[1, Offset + i] := rec.CompType;
    TheGrid.Cells[2, Offset + i] := rec.Caption;
    TheGrid.Cells[3, Offset + i] := rec.Accelerator;
    TheGrid.Cells[4, Offset + i] := rec.Shortcut;
    for j := 0 to Length(rec.SecondaryShortcuts) - 1 do begin
      TheGrid.Cells[5 + j, Offset + i] := rec.SecondaryShortcuts[j];
    end;
  end;

  TGrid_Resize(TheGrid, [roUseGridWidth, roUseAllRows])
end;

destructor Tf_FormHotkeys.Destroy;
begin

  inherited;
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
    cnv.Brush.Color := clhighlight;
  end else if gdFixed in State then begin
    cnv.Brush.Color := clBtnFace
  end else begin
    cnv.Brush.Color := sg.Color;
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

