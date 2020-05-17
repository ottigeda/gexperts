unit GX_FormHotkeys;

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
  u_dzTypes,
  Menus;

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
  Tf_FormHotkeys = class(TForm)
    l_HotAndAcceleratorKeys: TLabel;
    TheGrid: TStringGrid;
    TheActionList: TActionList;
    act_: TAction;
    pm_: TPopupMenu;
    SomeMenuItem1: TMenuItem;
    b_somebutton: TButton;
    procedure TheGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
      State: TGridDrawState);
  private
    FCompRecs: TComponentRecArr;
    procedure DrawStringGridCell(_sg: TStringGrid; _Text: string; const _Rect: TRect;
      _State: TGridDrawState; _Focused: Boolean; _Tag: Integer);
  public
    constructor Create(_Owner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

uses
  ToolsAPI,
  u_dzVclUtils,
  GX_OtaUtils,
  GX_Experts,
  TypInfo;

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
  TheGrid.Cells[3, 0] := 'Accellerator';
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
begin
  DrawStringGridCell(sg, sg.Cells[ACol, ARow], Rect, State, sg.Focused, Integer(sg.Objects[ACol, ARow]));
end;

procedure Tf_FormHotkeys.DrawStringGridCell(_sg: TStringGrid; _Text: string; const _Rect: TRect;
  _State: TGridDrawState; _Focused: Boolean; _Tag: Integer);
var
  cnv: TCanvas;
begin
  cnv := TheGrid.Canvas;
  if _Text = 'N/A' then begin
    _Text := '';
    if not (gdSelected in _State) then
      cnv.Brush.Color := clSilver;
  end;
{$IFDEF DEBUG_GRID_DRAWING}
//  SendDebugFmt('Drawing grid %s: DefaultRowHeight: %d Rect.Left: %d .Top: %d  .Width: %d .Height: %d',
//    [_sg.Name, _sg.DefaultRowHeight, _Rect.Left, _Rect.Top, _Rect.Right - _Rect.Left, _Rect.Bottom - _Rect.Top]);
{$ENDIF}
  cnv.FillRect(_Rect);
  cnv.TextRect(_Rect, _Rect.Left + 2, _Rect.Top + 2, _Text);
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
