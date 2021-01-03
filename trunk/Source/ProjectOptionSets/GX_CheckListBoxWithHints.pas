// Original Author: John Hansen <John_Hansen@tcsmgmt.com>
unit GX_CheckListBoxWithHints;

interface

uses
  SysUtils,
  Types,
  Classes,
  CheckLst,
  Messages,
  Controls;

type
  TBoolArray = array of Boolean;
  TJCHListSortCompare = function(_Lst: TStrings; const _ChkArr: TBoolArray; _Idx1, _Idx2: Integer): Integer of object;
  TGetHintEvent = procedure(Sender: TObject; const CursorPos: TPoint; var HintStr: string) of object;

  TCheckListBoxWithHints = class(TCheckListBox)
  private
    FOnGetHint: TGetHintEvent;
    FSortAscend: Boolean;
    FSortByString: Boolean;
    FMouseDownIndex: Integer;
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    procedure SetSortAscend(const Value: Boolean);
    procedure SetSortByString(const Value: Boolean);
    procedure QuickSort(_TmpList: TStrings; var _ChkArr: TBoolArray; l, R: Integer; SCompare: TJCHListSortCompare);
  protected
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DoOnGetHint(const CursorPos: TPoint; var HintStr: string); // virtual;
    function CompareByStringAscending(_Lst: TStrings; const _ChkArr: TBoolArray; _Idx1, _Idx2: Integer): Integer;
    function CompareByStringDescending(_Lst: TStrings; const _ChkArr: TBoolArray; _Idx1, _Idx2: Integer): Integer;
    function CompareByCheckAscending(_Lst: TStrings; const _ChkArr: TBoolArray; _Idx1, _Idx2: Integer): Integer;
    function CompareByCheckDescending(_Lst: TStrings; const _ChkArr: TBoolArray; _Idx1, _Idx2: Integer): Integer;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SortList(Compare: TJCHListSortCompare);
    procedure Resort;
    property SortAscending: Boolean read FSortAscend write SetSortAscend;
    property SortByString: Boolean read FSortByString write SetSortByString;
    property OnGetHint: TGetHintEvent read FOnGetHint write FOnGetHint;
  end;

implementation

uses
  Forms;

{ TCheckListBoxWithHints }

procedure TCheckListBoxWithHints.QuickSort(_TmpList: TStrings; var _ChkArr: TBoolArray; L, R: Integer; SCompare: TJCHListSortCompare);
var
  I, J, P: Integer;
  tmpChecked: Boolean;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while SCompare(_TmpList, _ChkArr, I, P) < 0 do Inc(I);
      while SCompare(_TmpList, _ChkArr, J, P) > 0 do Dec(J);
      if I <= J then
      begin
        // exchange I and J
        _TmpList.Exchange(i, j);
        tmpChecked := _ChkArr[i];
        _ChkArr[i] := _ChkArr[j];
        _ChkArr[j] := tmpChecked;
        if P = I then
          P := J
        else if P = J then
          P := I;

        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(_TmpList, _ChkArr, L, J, SCompare);
    L := I;
  until I >= R;
end;

procedure TCheckListBoxWithHints.CMHintShow(var Message: TMessage);
var
  NewHintStr: string;
begin
  NewHintStr := TCMHintShow(Message).HintInfo^.HintStr;
  DoOnGetHint(TCMHintShow(Message).HintInfo^.CursorPos, NewHintStr);
  TCMHintShow(Message).HintInfo^.HintStr := NewHintStr;
  inherited;
end;

constructor TCheckListBoxWithHints.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSortAscend := True;
  FSortByString := True;
  IntegralHeight := True;
  FMouseDownIndex := -1;
end;

procedure TCheckListBoxWithHints.DoOnGetHint(const CursorPos: TPoint; var HintStr: string);
begin
  if Assigned(FOnGetHint) then
    FOnGetHint(Self, CursorPos, HintStr);
end;

procedure TCheckListBoxWithHints.SetSortAscend(const Value: Boolean);
begin
  if Sorted then
    Exit;
  FSortAscend := Value;
  Resort;
end;

procedure TCheckListBoxWithHints.SetSortByString(const Value: Boolean);
begin
  if Sorted then
    Exit;
  FSortByString := Value;
  Resort;
end;

procedure TCheckListBoxWithHints.SortList(Compare: TJCHListSortCompare);
var
  LItems: TStrings;
  cnt: Integer;
  tmpList: TStringList;
  ChkArr: TBoolArray;
  i: Integer;
begin
  // Originally this worked on the Items and Checked properties which resulted in many calls involving
  // Windows messages. Now we take a copy of these properties, sort that copy and ssign the sorted result,
  // which is much faster.
  LItems := Items;
  cnt := LItems.Count;
  if cnt = 0 then
    Exit;

  tmpList := TStringList.Create;
  try
    tmpList.AddStrings(LItems);
    SetLength(ChkArr, cnt);
    for i := 0 to cnt - 1 do
      ChkArr[i] := Checked[i];
    QuickSort(tmpList, ChkArr, 0, cnt - 1, Compare);
    Items.BeginUpdate;
    try
      Items := tmpList;
      for i := 0 to cnt - 1 do
        Checked[i] := ChkArr[i];
    finally
      Items.EndUpdate;
    end;
  finally
    tmpList.Free;
  end;
end;

procedure TCheckListBoxWithHints.Resort;
begin
  if SortByString then
  begin
    if SortAscending then
      SortList(CompareByStringAscending)
    else
      SortList(CompareByStringDescending);
  end
  else
  begin
    if SortAscending then
      SortList(CompareByCheckAscending)
    else
      SortList(CompareByCheckDescending);
  end;
end;

function TCheckListBoxWithHints.CompareByCheckAscending(_Lst: TStrings; const _ChkArr: TBoolArray;
  _Idx1, _Idx2: Integer): Integer;
begin
  Result := 0;
  if _ChkArr[_Idx1] and not _ChkArr[_Idx2] then
    Result := -1
  else if _ChkArr[_Idx1] and _ChkArr[_Idx2] then
    Result := AnsiCompareText(_Lst[_Idx1], _Lst[_Idx2])
  else if not _ChkArr[_Idx1] and not _ChkArr[_Idx2] then
    Result := AnsiCompareText(_Lst[_Idx1], _Lst[_Idx2])
  else if not _ChkArr[_Idx1] and _ChkArr[_Idx2] then
    Result := 1;
end;

// todo: Isn't that simply -doCompareByCeckAscending ?
function TCheckListBoxWithHints.CompareByCheckDescending(_Lst: TStrings; const _ChkArr: TBoolArray;
  _Idx1, _Idx2: Integer): Integer;
begin
  Result := 0;
  if _ChkArr[_Idx1] and not _ChkArr[_Idx2] then
    Result := 1
  else if _ChkArr[_Idx1] and _ChkArr[_Idx2] then
    Result := AnsiCompareText(_Lst[_Idx2], _Lst[_Idx1])
  else if not _ChkArr[_Idx1] and not _ChkArr[_Idx2] then
    Result := AnsiCompareText(_Lst[_Idx2], _Lst[_Idx1])
  else if not _ChkArr[_Idx1] and _ChkArr[_Idx2] then
    Result := -1;
end;

function TCheckListBoxWithHints.CompareByStringAscending(_Lst: TStrings; const _ChkArr: TBoolArray;
  _Idx1, _Idx2: Integer): Integer;
begin
  Result := AnsiCompareText(_Lst[_Idx1], _Lst[_Idx2]);
end;

function TCheckListBoxWithHints.CompareByStringDescending(_Lst: TStrings; const _ChkArr: TBoolArray;
  _Idx1, _Idx2: Integer): Integer;
begin
  Result := AnsiCompareText(_Lst[_Idx2], _Lst[_Idx1]);
end;

procedure TCheckListBoxWithHints.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);

  // It would be very nice to have the hint permanently up,
  // and have it tracking the current item under the
  // mouse pointer.
end;

procedure TCheckListBoxWithHints.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  //Assert(FMouseDownIndex = -1);
  FMouseDownIndex := ItemAtPos(Point(X, Y), True);
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCheckListBoxWithHints.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FMouseDownIndex := -1;
  inherited MouseUp(Button, Shift, X, Y);
end;

end.
