unit GX_KeyboardShortcuts;

{$I GX_CondDefine.inc}

interface

uses
  Windows,
  SysUtils,
  Classes,
  Grids,
  Types,
  Controls,
  Forms,
  Dialogs,
  Graphics,
  GX_Experts,
  GX_BaseForm;

type
  TfmGxKeyboardShortcuts = class(TfmBaseForm)
    sg_Actions: TStringGrid;
    procedure sg_ActionsDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
      State: TGridDrawState);
    procedure sg_ActionsMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure sg_ActionsMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure sg_ActionsMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    FSortingCol: Integer;
    function ConfigurationKey: string;
    function CompareEntries(_Idx1, _Idx2: Integer): Integer;
    procedure SwapEntries(_Idx1, _Idx2: Integer);
    procedure DrawStringGridCell(_sg: TStringGrid; const _Text: string;
      const _Rect: TRect; _State: TGridDrawState; _Duplicate: Boolean);
    function ScrollGrid(_Grid: TStringGrid; _Direction: Integer; _Shift: TShiftState;
      _MousePos: TPoint): Boolean;
    function GetShortcut(_Idx: Integer): TShortCut;
  public
    class procedure Execute(_bmp: TBitmap);
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
{$IFOPT D+}GX_DbugIntf,
{$ENDIF}
  Menus,
  StrUtils,
  Math,
  Actions,
  ActnList,
  GX_GExperts,
  GX_ConfigurationInfo,
  GX_ActionBroker,
  GX_dzVclUtils,
  GX_dzQuicksort,
  GX_GenericUtils;

type
  TGxKeyboardShortcuts = class(TGX_Expert)
  private
  protected
  public
    function CanHaveShortCut: Boolean; override;
    constructor Create; override;
    destructor Destroy; override;
    function GetActionCaption: string; override;
    function GetHelpString: string; override;
    function HasConfigOptions: Boolean; override;
    procedure Execute(Sender: TObject); override;
  end;

{ TGxKeyboardShortcuts }

procedure TGxKeyboardShortcuts.Execute(Sender: TObject);
begin
  TfmGxKeyboardShortcuts.Execute(GetBitmap);
end;

function TGxKeyboardShortcuts.CanHaveShortCut: Boolean;
begin
  Result := False;
end;

constructor TGxKeyboardShortcuts.Create;
begin
  inherited Create;
end;

destructor TGxKeyboardShortcuts.Destroy;
begin
  inherited Destroy;
end;

function TGxKeyboardShortcuts.GetActionCaption: string;
resourcestring
  SMenuCaption = 'Keyboard Shortcuts';
begin
  Result := SMenuCaption;
end;

function TGxKeyboardShortcuts.GetHelpString: string;
resourcestring
  SSampleExpertHelp =
    'List all keyboard shortcuts of registered actions in the IDE';
begin
  Result := SSampleExpertHelp;
end;

function TGxKeyboardShortcuts.HasConfigOptions: Boolean;
begin
  Result := False;
end;

{ TfmGxKeyboardShortcuts }

class procedure TfmGxKeyboardShortcuts.Execute(_bmp: TBitmap);
var
  frm: TfmGxKeyboardShortcuts;
begin
  frm := TfmGxKeyboardShortcuts.Create(nil);
  try
    ConvertBitmapToIcon(_bmp, frm.Icon);
    frm.ShowModal;
  finally
    frm.Free;
  end;
end;

procedure TfmGxKeyboardShortcuts.FormClose(Sender: TObject; var Action: TCloseAction);
var
  Settings: IExpertSettings;
begin
  // Do not localize any of the following lines.
  Settings := ConfigInfo.GetExpertSettings(ConfigurationKey);
  Settings.SaveForm('Window', Self);
end;

function TfmGxKeyboardShortcuts.ScrollGrid(_Grid: TStringGrid; _Direction: Integer;
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

function TfmGxKeyboardShortcuts.ConfigurationKey: string;
begin
  Result := TGxKeyboardShortcuts.ConfigurationKey;
end;

constructor TfmGxKeyboardShortcuts.Create(_Owner: TComponent);
var
  TabStr: string;
  Row: Integer;

  procedure AppendLine(_Shortcut: TShortCut; _act: TCustomAction; _IsSecondary: Boolean = False);
  var
    Key: Word;
    Shift: TShiftState;
    KeyStr: string;
    ShiftStr: string;
  begin
    TGrid_SetNonfixedRowCount(sg_Actions, Row);
    ShortCutToKey(_Shortcut, Key, Shift);
    KeyStr := ShortCutToText(ShortCut(Key, []));
    if _IsSecondary then
      KeyStr := KeyStr + ' *';
    ShiftStr := ShortCutToText(ShortCut(VK_TAB, Shift));
    ShiftStr := LeftStr(ShiftStr, Length(ShiftStr) - Length(TabStr) - 1);
    sg_Actions.Objects[0, Row] := Pointer(_Shortcut);
    sg_Actions.Cells[0, Row] := ShiftStr;
    sg_Actions.Cells[1, Row] := KeyStr;
    sg_Actions.Cells[2, Row] := _act.Name;
    sg_Actions.Cells[3, Row] := _act.Caption;
    Inc(Row);
  end;

var
  i: Integer;
  ContAct: TContainedAction;
  act: TCustomAction;
  j: Integer;
  TheShortCut: TShortCut;
  s: string;
  Settings: IExpertSettings;
begin
  inherited;

  TControl_SetMinConstraints(Self);

  FSortingCol := 1;

  Settings := ConfigInfo.GetExpertSettings(ConfigurationKey);
  Settings.LoadForm('Window', Self);

  sg_Actions.Cells[0, 0] := 'Modifier';
  sg_Actions.Cells[1, 0] := 'Key';
  sg_Actions.Cells[2, 0] := 'Action';
  sg_Actions.Cells[3, 0] := 'Caption';

  sg_Actions.OnMouseWheelDown := sg_ActionsMouseWheelDown;
  sg_Actions.OnMouseWheelUp := sg_ActionsMouseWheelUp;

  TabStr := ShortCutToText(ShortCut(VK_TAB, []));
  Row := sg_Actions.FixedRows;
  for i := 0 to GxActionBroker.ActionCount - 1 do begin
    ContAct := GxActionBroker.Actions[i];
    if ContAct is TCustomAction then begin
      act := TCustomAction(ContAct);
      if act.ShortCut <> 0 then begin
        AppendLine(act.ShortCut, act);
      end;
      for j := 0 to act.SecondaryShortCuts.Count - 1 do begin
        TheShortCut := act.SecondaryShortCuts.ShortCuts[j];
        if TheShortCut = 0 then begin
          // somebody assigned an invalid shortcut like 'Shift-Ctrl-Enter' to some action
          // it's probably futile to try and convert the string to a shortcut, but we do
          // it anyway.
          s := act.SecondaryShortCuts.Strings[j];
{$IFOPT D+}
          SendDebugFmt('secondary shortcut for action %s is 0 (text: %s)', [act.Name, s]);
{$ENDIF}
          TheShortCut := TextToShortCut(s);
        end;
        if TheShortCut = 0 then begin
{$IFOPT D+}
          SendDebugFmt('secondary shortcut for action %s is still 0 after TextToShortcut', [act.Name]);
{$ENDIF}
        end else begin
          AppendLine(TheShortCut, act, True);
        end;
      end;
    end;
  end;

  QuickSort(sg_Actions.FixedRows, Row - 1, CompareEntries, SwapEntries);

  for i := sg_Actions.FixedRows to Row - 2 do begin
    if SameText(ShortCutToText(GetShortcut(i)), ShortCutToText(GetShortcut(i + 1))) then begin
      sg_Actions.Objects[1, i] := Pointer(1);
      sg_Actions.Objects[1, i + 1] := Pointer(1);
    end;
  end;

  TStringGrid_AdjustRowHeight(sg_Actions);
  TGrid_Resize(sg_Actions, [roUseGridWidth, roUseAllRows, roReduceMinWidth]);
end;

function TfmGxKeyboardShortcuts.GetShortcut(_Idx: Integer): TShortCut;
begin
  Result := Integer(sg_Actions.Objects[0, _Idx]);
end;

function ShiftToChar(_KeyIsSet: Boolean; _c: Char): Char;
begin
  if _KeyIsSet then
    Result := _c
  else
    Result := ' ';
end;

function ShiftToStr(_Shift: TShiftState): string;
begin
  // ssShift, ssAlt, ssCtrl,
  Result := ShiftToChar(ssShift in _Shift, 'S')
    + ShiftToChar(ssAlt in _Shift, 'A')
    + ShiftToChar(ssCtrl in _Shift, 'C');
end;

function KeyToStr(_Key: Word): string;
begin
  Result := ShortCutToText(ShortCut(_Key, []));
  Result := RightStr(StringOfChar(' ', 20) + Result, 20);
end;

function ShortcutToSortTextAlpha(_Shortcut: TShortCut): string;
var
  Key: Word;
  Shift: TShiftState;
begin
  ShortCutToKey(_Shortcut, Key, Shift);
  Result := ShiftToStr(Shift) + KeyToStr(Key);
end;

function ShortcutToSortTextMainKey(_Shortcut: TShortCut): string;
var
  Key: Word;
  Shift: TShiftState;
begin
  ShortCutToKey(_Shortcut, Key, Shift);
  Result := KeyToStr(Key) + ShiftToStr(Shift);
end;

function TfmGxKeyboardShortcuts.CompareEntries(_Idx1, _Idx2: Integer): Integer;
var
  s1: string;
  s2: string;
  Key1: Word;
  Key2: Word;
  Shift1: TShiftState;
  Shift2: TShiftState;
  c: Integer;
begin
  ShortCutToKey(GetShortcut(_Idx1), Key1, Shift1);
  ShortCutToKey(GetShortcut(_Idx2), Key2, Shift2);
  c := Abs(FSortingCol) - 1;
  case c of
    1: begin
        s1 := KeyToStr(Key1);
        s2 := KeyToStr(Key2);
      end;
    2, 3: begin
        s1 := sg_Actions.Cells[c, _Idx1];
        s2 := sg_Actions.Cells[c, _Idx2];
      end;
  else // 0
    s1 := ShiftToStr(Shift1);
    s2 := ShiftToStr(Shift2);
  end;
  s1 := s1 + '@' + sg_Actions.Rows[_Idx1].Text;
  s2 := s2 + '@' + sg_Actions.Rows[_Idx2].Text;
  Result := Sign(FSortingCol) * CompareText(s1, s2);
end;

procedure TfmGxKeyboardShortcuts.SwapEntries(_Idx1, _Idx2: Integer);
var
  s: string;
  c: Integer;
  p: Pointer;
begin
  for c := 0 to sg_Actions.ColCount - 1 do begin
    s := sg_Actions.Cells[c, _Idx1];
    sg_Actions.Cells[c, _Idx1] := sg_Actions.Cells[c, _Idx2];
    sg_Actions.Cells[c, _Idx2] := s;
  end;
  p := sg_Actions.Objects[0, _Idx1];
  sg_Actions.Objects[0, _Idx1] := sg_Actions.Objects[0, _Idx2];
  sg_Actions.Objects[0, _Idx2] := p;

  p := sg_Actions.Objects[1, _Idx1];
  sg_Actions.Objects[1, _Idx1] := sg_Actions.Objects[1, _Idx2];
  sg_Actions.Objects[1, _Idx2] := p;
end;

procedure TfmGxKeyboardShortcuts.DrawStringGridCell(_sg: TStringGrid; const _Text: string;
  const _Rect: TRect; _State: TGridDrawState; _Duplicate: Boolean);
var
  cnv: TCanvas;
begin
  cnv := _sg.Canvas;
  if _Duplicate then begin
    cnv.Brush.Color := clYellow;
    cnv.Font.Color := clBlack;
  end;
  cnv.FillRect(_Rect);
  cnv.TextRect(_Rect, _Rect.Left + 2, _Rect.Top + 2, _Text);
end;

procedure TfmGxKeyboardShortcuts.sg_ActionsDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var
  sg: TStringGrid absolute Sender;
begin
  DrawStringGridCell(sg, sg.Cells[ACol, ARow], Rect, State, LongBool(sg.Objects[1, ARow]));
end;

procedure TfmGxKeyboardShortcuts.sg_ActionsMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  c: Integer;
  r: Integer;
begin
  sg_Actions.MouseToCell(X, Y, c, r);
  if r < sg_Actions.FixedRows then begin
    Inc(c);
    if c = Abs(FSortingCol) then
      FSortingCol := -FSortingCol
    else
      FSortingCol := c;
    QuickSort(sg_Actions.FixedRows, sg_Actions.RowCount - 1, CompareEntries, SwapEntries);
  end;
end;

procedure TfmGxKeyboardShortcuts.sg_ActionsMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if (Sender is TStringGrid) then
    if ScrollGrid(TStringGrid(Sender), +1, Shift, MousePos) then
      Handled := True;
end;

procedure TfmGxKeyboardShortcuts.sg_ActionsMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if Sender is TStringGrid then
    if ScrollGrid(TStringGrid(Sender), -1, Shift, MousePos) then
      Handled := True;
end;

initialization
  RegisterGX_Expert(TGxKeyboardShortcuts);
end.
